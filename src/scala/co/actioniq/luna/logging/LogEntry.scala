package co.actioniq.luna.logging

import TransactionAction.TransactionAction


trait LogEntry {
  protected val action: TransactionAction
}
