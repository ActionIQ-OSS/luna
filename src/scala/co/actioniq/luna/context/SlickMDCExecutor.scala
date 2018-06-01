package slick.util

import java.lang.management.ManagementFactory
import java.util.concurrent._
import javax.management.{InstanceNotFoundException, ObjectName}

import scala.concurrent.duration._
import scala.concurrent._
import java.util.concurrent.atomic.AtomicInteger

import slick.util.AsyncExecutor.WithConnection

import scala.util.control.NonFatal

/**
  * Mostly copied from slick source code.  Modified to use MDC Context
  */
class DefaultSlickMDCExecutor extends SlickExecutor {
  override def getSlickContext(delegate: ExecutionContext): SlickMDCContext = new SlickMDCContext(delegate)
}
trait SlickExecutor extends Logging with SlickMDCContext.Provider {

  def apply(
    name: String,
    numThreads: Int,
    queueSize: Int
  ): AsyncExecutor = apply(name, numThreads, numThreads, queueSize)

  def apply(
    name: String,
    minThreads: Int,
    maxThreads: Int,
    queueSize: Int,
    maxConnections: Int = Integer.MAX_VALUE,
    keepAliveTime: Duration = 1.minute,
    registerMbeans: Boolean = false
  ): AsyncExecutor = new AsyncExecutor {
    @volatile private[this] lazy val mbeanName = new ObjectName(s"slick:type=AsyncExecutor,name=$name");

    // Before init: 0, during init: 1, after init: 2, during/after shutdown: 3
    private[this] val state = new AtomicInteger(0)

    @volatile private[this] var executor: ThreadPoolExecutor = _

    lazy val executionContext = {
      if (!state.compareAndSet(0, 1)) {
        throw new IllegalStateException("Cannot initialize ExecutionContext; AsyncExecutor already shut down")
      }
      val queue: BlockingQueue[Runnable] = queueSize match {
        case 0 => new SynchronousQueue[Runnable]
        case -1 => new LinkedBlockingQueue[Runnable]
        case n: Int => new SlickManagedArrayBlockingQueue(maxConnections, n).asInstanceOf[BlockingQueue[Runnable]]
      }
      val tf = new DaemonThreadFactory(name + "-")
      executor = new ThreadPoolExecutor(
        minThreads,
        maxThreads,
        keepAliveTime.toMillis,
        TimeUnit.MILLISECONDS,
        queue,
        tf
      ) {

        /** If the runnable/task is a low/medium priority item, we increase the items in use count,
          * because first thing it will do is open a Jdbc connection from the pool. */
        override def beforeExecute(t: Thread, r: Runnable): Unit = {
          (r, queue) match {
            case (pr: PriorityRunnableProxy, q: SlickManagedArrayBlockingQueue[Runnable])
              if pr.priority != WithConnection => q.increaseInUseCount(pr)
            case _ =>
          }
          super.beforeExecute(t, r)
        }

        /**
          * If the runnable/task has released the Jdbc connection we decrease the counter again
          */
        override def afterExecute(r: Runnable, t: Throwable): Unit = {
          super.afterExecute(r, t)
          (r, queue) match {
            case (pr: PriorityRunnableProxy, q: SlickManagedArrayBlockingQueue[Runnable]) if pr.connectionReleased  =>
              q.decreaseInUseCount()
            case _ =>
          }
        }

      }
      if (registerMbeans) {
        try {
          val mbeanServer = ManagementFactory.getPlatformMBeanServer
          if (mbeanServer.isRegistered(mbeanName)) {
            logger.warn(s"MBean $mbeanName already registered (AsyncExecutor names should be unique)")
          } else {
            logger.debug(s"Registering MBean $mbeanName")
            mbeanServer.registerMBean(new AsyncExecutorMXBean {
              def getMaxQueueSize = queueSize
              def getQueueSize = queue.size()
              def getMaxThreads = maxThreads
              def getActiveThreads = executor.getActiveCount
            }, mbeanName)
          }
        } catch { case NonFatal(ex) => logger.error("Error registering MBean", ex) }
      }
      if (!state.compareAndSet(1, 2)) {
        unregisterMbeans()
        executor.shutdownNow()
        throw new IllegalStateException(
          "Cannot initialize ExecutionContext; AsyncExecutor shut down during initialization"
        )
      }
      val context = new ExecutionContextExecutor {
        override def reportFailure(t: Throwable): Unit = loggingReporter(t)

        override def execute(command: Runnable): Unit = {
          executor.execute(command)
        }
      }
      getSlickContext(context)
    }

    private[this] def unregisterMbeans(): Unit = if (registerMbeans) {
      try {
        val mbeanServer = ManagementFactory.getPlatformMBeanServer
        logger.debug(s"Unregistering MBean $mbeanName")
        try mbeanServer.unregisterMBean(mbeanName) catch { case _: InstanceNotFoundException => }
      } catch { case NonFatal(ex) => logger.error("Error unregistering MBean", ex) }
    }

    def close(): Unit = if (state.getAndSet(3) == 2) {
      unregisterMbeans()
      executor.shutdownNow()
      if (!executor.awaitTermination(30, TimeUnit.SECONDS)) {
        logger.warn("Abandoning ThreadPoolExecutor (not yet destroyed after 30 seconds)")
      }
    }
  }

  val loggingReporter: Throwable => Unit = (t: Throwable) => {
    logger.warn("Execution of asynchronous I/O action failed", t)
  }

  private class DaemonThreadFactory(namePrefix: String) extends ThreadFactory {
    private[this] val group = Option(System.getSecurityManager)
      .fold(Thread.currentThread.getThreadGroup)(_.getThreadGroup)
    private[this] val threadNumber = new AtomicInteger(1)

    def newThread(r: Runnable): Thread = {
      val t = new Thread(group, r, namePrefix + threadNumber.getAndIncrement, 0)
      if (!t.isDaemon) {
        t.setDaemon(true)
      }
      if (t.getPriority != Thread.NORM_PRIORITY) {
        t.setPriority(Thread.NORM_PRIORITY)
      }
      t
    }
  }
}
