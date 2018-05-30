package co.actioniq.luna.example

import co.actioniq.luna.dao.DbUUID
import co.actioniq.luna.logging.LogEntry
import co.actioniq.luna.logging.TransactionAction.TransactionAction

case class LoggingModel(override val action: TransactionAction, id: DbUUID, name: String) extends LogEntry
