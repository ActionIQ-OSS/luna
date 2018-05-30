package co.actioniq.luna

import co.actioniq.luna.OptionCompareOption.optionCompare
import co.actioniq.luna.dao.{DAOLongIdQuery, DbLongOptId, FormValidatorMessageSeq, H2DAO, H2DAOTable, IdModel, JdbcTypeImplicits, JdbcTypes}
import co.actioniq.luna.logging.{NoopBackend, TransactionLogger}
import org.junit.runner.RunWith
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import slick.jdbc.{H2Profile, JdbcProfile}
import slick.lifted.{TableQuery, Tag}
import slick.jdbc.H2Profile.api._

import scala.concurrent.ExecutionContext

@RunWith(classOf[JUnitRunner])
class AiqSlickProfileSpec extends Specification with Mockito {
  implicit val logger = new NoopBackend {}

  "AiqDriver" should {
    "Be able to delete" in new TestScope  {
      dao.delete() mustEqual """delete from "persistedslick" where "persistedslick"."id" = 3"""
    }
    "Handle option if defined" in new TestScope {
      dao.readByIdIfDefined(
        Some(12)
      ) mustEqual """select "id", "some_id", "name" from "persistedslick" where "some_id" = 12"""
    }
    "Handle option if not defined" in new TestScope {
      dao.readByIdIfDefined(None) mustEqual """select "id", "some_id", "name" from "persistedslick""""
    }
    "Handle option compare with null" in new TestScope {
      dao.readByIdCompare(
        None
      ) mustEqual """select "id", "some_id", "name" from "persistedslick" where "some_id" is null"""
    }
  }

  trait TestScope extends SlickScope with PersistedSlick.Provider {
    val dao = new PersistedDao(
      db,
      persistedSlick
    )
  }

  case class PersistedSlick(
    override val id: DbLongOptId,
    someId: Option[Long],
    name: String
  ) extends IdModel[DbLongOptId] with JdbcTypeImplicits.h2JdbcTypeImplicits.DbImplicits

  class PersistedSlickTable(tag: Tag)
    extends H2DAOTable[PersistedSlick, DbLongOptId](tag, "persistedslick") {

    override def id: Rep[DbLongOptId] = column[DbLongOptId]("id")
    def someId: Rep[Option[Long]] = column[Option[Long]]("some_id")
    def name: Rep[String] = column[String]("name")

    override def * = ( // scalastyle:ignore
      id,
      someId,
      name
    ) <> (PersistedSlick.tupled, PersistedSlick.unapply)
  }

  object PersistedSlick {
    trait Provider {
      private val creator = (tag: Tag) => new PersistedSlickTable(tag)
      val persistedSlick = new TableQuery[PersistedSlickTable](creator)
    }
    def tupled = (PersistedSlick.apply _).tupled // scalastyle:ignore
  }

  class PersistedDao(
    override val db: DBWithLogging,
    override val slickQuery: TableQuery[PersistedSlickTable]
  ) extends H2DAO[PersistedSlickTable, PersistedSlick, DbLongOptId]
    with NoopBackend
    with DAOLongIdQuery[PersistedSlickTable, PersistedSlick, H2Profile] {

    def readByIdIfDefined(id: Option[Long]): String = {
      slickQuery.filter(s => optionCompare(s.someId) =:= id).result.statements.toList.head
    }

    def readByIdCompare(id: Option[Long]): String = {
      slickQuery.filter(s => optionCompare(s.someId) =?= id).result.statements.toList.head
    }

    def delete(): String = {
      slickQuery.filter(v => optLongCompare(v.id) equalsLong  3L)
        .delete.statements.toList.head
    }

    override protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

    override protected def addCreateTransaction(id: DbLongOptId, input: PersistedSlick): Unit = ???

    override protected def addUpdateTransaction(
      id: DbLongOptId,
      input: PersistedSlick,
      original: PersistedSlick
    ): Unit = ???

    override protected def addDeleteTransaction(id: DbLongOptId, original: PersistedSlick): Unit = ???

    override def nameSingle: String = ???

    override def validateCreate(
      input: PersistedSlick
    )(implicit ec: ExecutionContext): DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read] = ???

    override def validateUpdate(
      input: PersistedSlick,
      original: PersistedSlick
    )(implicit ec: ExecutionContext): DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read] = ???
  }
}
