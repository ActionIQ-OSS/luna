package co.actioniq.luna.dao

import slick.jdbc.JdbcBackend.Database
import slick.jdbc.JdbcProfile

import scala.concurrent.ExecutionContext

class NoopFormValidator[T](override protected val db: Database, override protected val profile: JdbcProfile)
  extends DAOFormValidator[T]{
  import profile.api._ // scalastyle:ignore
  override def validateCreate(input: T)(implicit ec: ExecutionContext):
  DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read] = {
    DBIO.successful(FormValidatorMessageSeq())
  }

  override def validateUpdate(input: T, original: T)(implicit ec: ExecutionContext):
  DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read] = {
    DBIO.successful(FormValidatorMessageSeq())
  }
}
