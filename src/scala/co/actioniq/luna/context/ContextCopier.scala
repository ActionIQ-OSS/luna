package co.actioniq.luna.context

import org.slf4j.MDC

trait ContextCopier {
  protected def setContext(context: Option[java.util.Map[String, String]]): Unit = {
    if (context.isEmpty) {
      MDC.clear()
    } else {
      MDC.setContextMap(context.get)
    }
  }

  protected def getRunnableContext(
    mdcContext: Option[java.util.Map[String, String]],
    r: Runnable
  ): Runnable = {
    new Runnable {
      override def run(): Unit = {
        // backup the callee MDC context
        val oldMDCContext = Option(MDC.getCopyOfContextMap)
        // Run the runnable with the captured context
        setContext(mdcContext)
        try {
          r.run()
        } finally {
          // restore the callee MDC context
          setContext(oldMDCContext)
        }
      }
    }
  }
}
