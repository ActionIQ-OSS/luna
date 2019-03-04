package co.actioniq.luna.example

import co.actioniq.luna.DBWithLogging
import co.actioniq.luna.dao.{DAOH2Profile, DAOLongIdQuery, DbLongOptId, FormValidatorMessageSeq, H2DAO}
import co.actioniq.luna.dao.DAOH2Profile.api._
import co.actioniq.luna.logging.NoopBackend
import slick.dbio.DBIOAction
import slick.lifted.TableQuery
import slick.util.SlickMDCContext

import scala.concurrent.ExecutionContext

class TeamDAO(
  override val db: DBWithLogging,
  override val slickQuery: TableQuery[TeamTable]
) extends H2DAO[TeamTable, Team, DbLongOptId]
  with NoopBackend
  with DAOLongIdQuery[TeamTable, Team, DAOH2Profile] {

  def readByIdQueryStatement(id: DbLongOptId): String = readByIdQuery(id).result.statements.head

  override protected implicit val ec: ExecutionContext = SlickMDCContext.Implicits.defaultContext

  override protected def addCreateTransaction(id: DbLongOptId, input: Team): Unit = {}

  override protected def addUpdateTransaction(id: DbLongOptId, input: Team, original: Team): Unit = {}

  override protected def addDeleteTransaction(id: DbLongOptId, original: Team): Unit = {}

  override def nameSingle: String = ???

  override def validateCreate(
    input: Team
  )(implicit ec: ExecutionContext): DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read] = {
    val errors = FormValidatorMessageSeq()
    for {
      dupes <- readAction(q => q.filter(_.name === input.name))
      _ = errors.assert(dupes.isEmpty, "Name should be unique")
    } yield errors
  }

  override def validateUpdate(
    input: Team,
    original: Team
  )(implicit ec: ExecutionContext): DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read] = {
    val errors = FormValidatorMessageSeq()
    errors.assert(input.name != "Harry", "Name should not be Harry")
    DBIO.successful(errors)
  }
}
