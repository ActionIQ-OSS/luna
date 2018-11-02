package co.actioniq.luna.example

import co.actioniq.luna.DBWithLogging
import co.actioniq.luna.dao._
import co.actioniq.luna.logging.NoopBackend
import slick.dbio.Effect
import slick.jdbc.{H2Profile, JdbcType}
import slick.lifted.TableQuery
import slick.util.SlickMDCContext
import slick.jdbc.H2Profile.api._

import scala.concurrent.ExecutionContext

class KittyDAO(
  override val db: DBWithLogging,
  override val slickQuery: TableQuery[KittyTable]
) extends H2DAO[KittyTable, Kitty, DbUUID]
  with NoopBackend
  with DAOUUIDQuery[KittyTable, Kitty, H2Profile]
  with SoftDeleteOptional[KittyTable, Kitty, DbUUID, H2Profile, Long]{

  override protected implicit val ec: ExecutionContext = SlickMDCContext.Implicits.defaultContext

  override def nameSingle: String = ???

  override protected def addCreateTransaction(id: DbUUID, input: Kitty): Unit = {}

  override protected def addUpdateTransaction(id: DbUUID, input: Kitty, original: Kitty): Unit = {}

  override protected def addDeleteTransaction(id: DbUUID, original: Kitty): Unit = {}

  override def validateCreate(
    input: Kitty
  )(implicit ec: ExecutionContext): DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read] = {
    DBIO.successful(FormValidatorMessageSeq())
  }

  override def validateUpdate(
    input: Kitty,
    original: Kitty
  )(implicit ec: ExecutionContext): DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read] = {
    DBIO.successful(FormValidatorMessageSeq())
  }

  override def softNotDeletedFilter(t: KittyTable) = t.deletedAt.isEmpty

  override def softDeleteFilterField(t: KittyTable)(implicit fType: JdbcType[Long]) = t.deletedAt

  override def softDeleteFilterUpdate() = Some(12345)
}
