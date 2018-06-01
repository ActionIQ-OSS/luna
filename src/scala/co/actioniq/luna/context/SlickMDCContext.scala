package slick.util


import co.actioniq.luna.context.ContextCopier
import org.slf4j.MDC
import slick.util.AsyncExecutor.{PrioritizedRunnable, Priority, WithConnection}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

object SlickMDCContext {
  def fromThread(delegate: ExecutionContext): ExecutionContextExecutor = {
    new SlickMDCContext(delegate)
  }
  object Implicits {
    implicit lazy val defaultContext: ExecutionContext = {
      SlickMDCContext.fromThread(scala.concurrent.ExecutionContext.Implicits.global)
    }
  }
  trait Provider {
    def getSlickContext(delegate: ExecutionContext): SlickMDCContext
  }
}

/**
  * Manages execution to ensure that the given MDC context are set correctly
  * in the current thread. Actual execution is performed by a delegate ExecutionContext.
  */
class SlickMDCContext(delegate: ExecutionContext)
  extends ExecutionContextExecutor {
  def execute(runnable: Runnable): Unit = {
    val mdcContext = Option(MDC.getCopyOfContextMap)
    val copy = runnable match {
      case priority: PrioritizedRunnable => getRunnableCopyContextPriority(mdcContext, priority)
      case _ => getRunnableCopyContextPriority(mdcContext, runnable)
    }
    delegate.execute(copy)
  }
  def reportFailure(t: Throwable): Unit = delegate.reportFailure(t)

  protected def getRunnableCopyContextPriority(
    mdcContext: Option[java.util.Map[String, String]],
    r: Runnable
  ): Runnable = {
    val priority = new PrioritizedRunnable {
      override val priority: Priority = WithConnection
      override def run(): Unit = r.run()
    }
    getRunnableCopyContextPriority(mdcContext, priority)
  }

  protected def getRunnableCopyContextPriority(
    mdcContext: Option[java.util.Map[String, String]],
    r: PrioritizedRunnable
  ): Runnable = {
    new PriorityRunnableProxy(r, mdcContext)
  }
}

class PriorityRunnableProxy(
  val self: PrioritizedRunnable,
  mdcContext: Option[java.util.Map[String, String]]
) extends Proxy with Runnable with ContextCopier  {
  def connectionReleased: Boolean = self.connectionReleased
  def connectionRelease_= (input: Boolean): Unit = { //scalastyle:ignore
    self.connectionReleased = input
  }
  def inUseCounterSet: Boolean = self.inUseCounterSet
  def inUseCounterSet_=(input: Boolean): Unit = { //scalastyle:ignore
    self.inUseCounterSet = input
  }
  def priority: Priority = self.priority
  def run(): Unit = {
    // backup the callee MDC context
    val oldMDCContext = Option(MDC.getCopyOfContextMap)
    // Run the runnable with the captured context
    setContext(mdcContext)
    try {
      self.run()
    } finally {
      // restore the callee MDC context
      setContext(oldMDCContext)
    }
  }
}
