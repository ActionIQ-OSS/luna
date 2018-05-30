package co.actioniq.luna.logging


trait TransactionLogger {
  def write[T <: LogEntry](data: T): Unit
  def flush(): Unit
  def clear(): Unit
}

trait NoopBackend extends TransactionLogger {
  override def write[T <: LogEntry](data: T): Unit = Unit

  override def flush(): Unit = Unit

  override def clear(): Unit = Unit
}
