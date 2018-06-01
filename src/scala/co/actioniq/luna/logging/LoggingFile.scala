package co.actioniq.luna.logging

import java.util.UUID

import grizzled.slf4j.Logger

import scala.collection.mutable.ListBuffer

trait LoggingFile extends TransactionLogger {
  protected final lazy val transLogger: Logger = Logger("co.actioniq.slick.logging.LoggingFile") //scalastyle:ignore
  val changes: ListBuffer[LogEntry] = ListBuffer()
  override def write[T <: LogEntry] (data: T): Unit = {
    this.synchronized {
      changes += data
    }
  }

  override def clear(): Unit = {
    this.synchronized{
      changes.clear()
    }
  }

  override def flush(): Unit = {
    val trxId = UUID.randomUUID().toString
    val localList = this.synchronized {
      val temp = changes.toList
      changes.clear()
      temp
    }
    localList.foreach(change => {
      val changeWithTrxId = s"$trxId - $change"
      transLogger.info(changeWithTrxId)
    })
  }

}
