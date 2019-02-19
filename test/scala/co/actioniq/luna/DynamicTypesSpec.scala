package co.actioniq.luna

import co.actioniq.luna.dao.{CoolH2Profile, DAO, DAODynamicProfileTable, DAOLongIdQuery, DbLongOptId, FormValidatorMessageSeq, IdModel, JdbcTypeImplicits, JdbcTypes}
import co.actioniq.luna.logging.NoopBackend
import co.actioniq.luna.OptionCompareOption.optionCompare
import org.junit.runner.RunWith
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import slick.jdbc.{H2Profile, JdbcProfile, MySQLProfile}
import slick.lifted.{TableQuery, Tag}

import scala.concurrent.ExecutionContext

@RunWith(classOf[JUnitRunner])
class DynamicTypesSpec extends Specification with Mockito {
  "DAOTable" should {
    "Allow dynamic h2" in new H2TestScope {
      dao.readQueryForTest mustEqual """select "id", "some_id", "name" from "persistedslick" where "id" = ?"""
    }
    "Allow dynamic mysql" in new MysqlTestScope {
      dao.readQueryForTest mustEqual "select `id`, `some_id`, `name` from `persistedslick` where `id` = ?"
    }
  }

  trait H2TestScope extends SlickScope with DynamicSlick.H2Provider {
    implicit val logger = new NoopBackend {}
    val dao = new PersistedDao(
      db,
      CoolH2Profile,
      dynamicSlick
    ) with JdbcTypeImplicits.h2JdbcTypeImplicits.DbImplicits
  }

  trait MysqlTestScope extends SlickScope with DynamicSlick.MysqlProvider {
    implicit val logger = new NoopBackend {}
    val dao = new PersistedDao(
      db,
      MySQLProfile,
      dynamicSlick
    ) with JdbcTypeImplicits.mySQLJdbcTypeImplicits.DbImplicits
  }

  case class DynamicSlick(
    override val id: DbLongOptId,
    someId: Option[Long],
    name: String
  ) extends IdModel[DbLongOptId]

  class DynamicWrapper[P <: JdbcProfile] (
    override val profile: P,
    override val implicits: JdbcTypeImplicits[P]
  ) extends DAODynamicProfileTable(profile, implicits){

    class DynamicSlickTable(tag: Tag)
      extends DAOTable[DynamicSlick, DbLongOptId](tag, "persistedslick") {
      import profile.api._

      override def id: Rep[DbLongOptId] = column[DbLongOptId]("id")
      def someId: Rep[Option[Long]] = column[Option[Long]]("some_id")
      def name: Rep[String] = column[String]("name")

      override def * = ( // scalastyle:ignore
        id,
        someId,
        name
      ) <> (DynamicSlick.tupled, DynamicSlick.unapply)
    }

  }


  object DynamicSlick {
    trait H2Provider {
      private val creator = (tag: Tag) => {
        val wrapper = new DynamicWrapper(CoolH2Profile, JdbcTypeImplicits.h2JdbcTypeImplicits)
          new wrapper.DynamicSlickTable(tag)
      }
      val dynamicSlick = new TableQuery[DynamicWrapper[CoolH2Profile]#DynamicSlickTable](creator)
    }
    trait MysqlProvider {
      private val creator = (tag: Tag) => {
        val wrapper = new DynamicWrapper(MySQLProfile, JdbcTypeImplicits.mySQLJdbcTypeImplicits)
        new wrapper.DynamicSlickTable(tag)
      }
      val dynamicSlick = new TableQuery[DynamicWrapper[MySQLProfile]#DynamicSlickTable](creator)
    }
    def tupled = (DynamicSlick.apply _).tupled // scalastyle:ignore
  }

  abstract class PersistedDao[P <: JdbcProfile](
    override val db: DBWithLogging,
    override val profile: P,
    override val slickQuery: TableQuery[DynamicWrapper[P]#DynamicSlickTable]
  ) extends DAO[DynamicWrapper[P]#DynamicSlickTable, DynamicSlick, DbLongOptId, P]
    with NoopBackend
    with DAOLongIdQuery[DynamicWrapper[P]#DynamicSlickTable, DynamicSlick, P] {
    import profile.api._

    def readByIdIfDefined(id: Option[Long]): String = {
      slickQuery.filter(s => optionCompare(s.someId) =:= id).result.statements.toList.head
    }

    def readByIdCompare(id: Option[Long]): String = {
      slickQuery.filter(s => optionCompare(s.someId) =?= id).result.statements.toList.head
    }

    def readQueryForTest: String = {
      slickQuery.filter(s => s.id === DbLongOptId(3)).result.statements.toList.head
    }


    override protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

    override protected def addCreateTransaction(id: DbLongOptId, input: DynamicSlick): Unit = ???

    override protected def addUpdateTransaction(
      id: DbLongOptId,
      input: DynamicSlick,
      original: DynamicSlick
    ): Unit = ???

    override protected def addDeleteTransaction(id: DbLongOptId, original: DynamicSlick): Unit = ???

    override def nameSingle: String = ???

    override def validateCreate(
      input: DynamicSlick
    )(implicit ec: ExecutionContext): DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read] = ???

    override def validateUpdate(
      input: DynamicSlick,
      original: DynamicSlick
    )(implicit ec: ExecutionContext): DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read] = ???
  }

}
