package co.actioniq.luna

import co.actioniq.luna.logging.TransactionLogger
import slick.jdbc.JdbcBackend.Database

import scala.concurrent.ExecutionContext

class DBWithLogging(val self: Database, val transactionLogger: TransactionLogger)
  extends Proxy {
  def run[R](a: slick.dbio.DBIOAction[R, slick.dbio.NoStream, scala.Nothing])(implicit ec: ExecutionContext):
  scala.concurrent.Future[R] = {
    self.run(a).transform(result => {
      transactionLogger.flush()
      result
    }, failed => {
      transactionLogger.clear()
      failed
    })
  }
}

object DBWithLogging {
  def apply(
    db: Database,
    transactionLogger: TransactionLogger
  ): DBWithLogging = new DBWithLogging(db, transactionLogger)
  implicit def database2DbWithLogging(db: Database)(implicit transactionLogger: TransactionLogger): DBWithLogging =
    DBWithLogging(db, transactionLogger)
  implicit def dBWithLogging2Database(slickDb: DBWithLogging): Database = slickDb.self
}
