package co.actioniq.aiqslick

import java.util.concurrent.TimeUnit

import co.actioniq.luna.{DBWithLogging, SlickScope}
import co.actioniq.luna.compiled.{BoundedSeq, BoundedSeq100, InSeqDbUUIDImplicits, SlickCompiledFunctionSingleton}
import co.actioniq.luna.dao.{DAOUUIDQuery, DbUUID, FormValidatorMessageSeq, H2DAO, H2DAOTable, IdModel, JdbcTypeImplicits}
import co.actioniq.luna.logging.NoopBackend
import org.junit.runner.RunWith
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import slick.jdbc.{H2Profile, JdbcProfile}
import slick.lifted.{AppliedCompiledFunction, CompiledFunction, CompiledStreamingExecutable, TableQuery, Tag}
import slick.jdbc.H2Profile.api._

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.{Duration, SECONDS}

@RunWith(classOf[JUnitRunner])
class CompiledQuerySpec extends Specification with Mockito {
  "compile query" should {
    "handle one input" in new TestScope {
      val query = dao.compiledQuery(BoundedSeq100(Seq(id)))
      val expectedQuery = """select "id", "name" from "persistedslick" where "id" in (""" +
        questions +
      ")"
      query.result.statements.head mustEqual expectedQuery
      val result = scala.concurrent.Await.result(db.run(query.result), Duration(20, SECONDS))
      result.size mustEqual 1
      result.head mustEqual PersistedSlick(id, "larry")
    }
    "handle surrounding input" in new TestScope {
      val query = dao.compiledQuerySurround(BoundedSeq100(Seq(id)))
      val expectedQuery = """select "id", "name" from "persistedslick" where ("id" in (""" +
        questions +
        ")) and ?"
      query.result.statements.head mustEqual expectedQuery
      val result = scala.concurrent.Await.result(db.run(query.result), Duration(20, SECONDS))
      result.size mustEqual 1
      result.head mustEqual PersistedSlick(id, "larry")
    }
    "get multiple rows" in new TestScope {
      val query = dao.compiledQuery(BoundedSeq100(Seq(id, id2)))
      val result = scala.concurrent.Await.result(db.run(query.result), Duration(20, SECONDS))
      result.size mustEqual 2
    }
    "error on 200" in new TestScope {
      val ids = (1 to 200).map(DbUUID(_))
      dao.compiledQuery(BoundedSeq100(ids)) must throwA[Exception]
    }
    "handles dynamic growing to 200" in new TestScope {
      val ids = (1 to 199).map(DbUUID(_)) :+ id
      val query = dao.getAppliedCompiledQueryIn(ids)
      val questions200 = (1 to 200).map {_ =>
        "?"
      }.mkString(",")
      val expectedQuery = """select "id", "name" from "persistedslick" where "id" in (""" +
        questions200 +
        ")"
      query.result.statements.head mustEqual expectedQuery
      val result = scala.concurrent.Await.result(db.run(query.result), Duration(20, SECONDS))
      result.size mustEqual 1
      result.head mustEqual PersistedSlick(id, "larry")
    }
    "handles dynamic growing to 500" in new TestScope {
      val ids = (1 to 399).map(DbUUID(_)) :+ id
      val query = dao.getAppliedCompiledQueryIn(ids)
      val questions500 = (1 to 500).map {_ =>
        "?"
      }.mkString(",")
      val expectedQuery = """select "id", "name" from "persistedslick" where "id" in (""" +
        questions500 +
        ")"
      query.result.statements.head mustEqual expectedQuery
      val result = scala.concurrent.Await.result(db.run(query.result), Duration(20, SECONDS))
      result.size mustEqual 1
      result.head mustEqual PersistedSlick(id, "larry")
    }
    "handles dynamic growing to 1000" in new TestScope {
      val ids = (1 to 999).map(DbUUID(_)) :+ id
      val query = dao.getAppliedCompiledQueryIn(ids)
      val questions1000 = (1 to 1000).map {_ =>
        "?"
      }.mkString(",")
      val expectedQuery = """select "id", "name" from "persistedslick" where "id" in (""" +
        questions1000 +
        ")"
      query.result.statements.head mustEqual expectedQuery
      val result = scala.concurrent.Await.result(db.run(query.result), Duration(20, SECONDS))
      result.size mustEqual 1
      result.head mustEqual PersistedSlick(id, "larry")
    }
    "maxes at 1000" in new TestScope {
      val ids = (1 to 1001).map(DbUUID(_))
      try {
        dao.getAppliedCompiledQueryIn(ids)
        failure("Query should throw exception")
      } catch {
        case e: Exception => e.getMessage mustEqual "Queries cannot handle more than 1000 items"
      }
    }
  }
  "singleton" should {
    "only instantiate once" in new TestScope {
      val query1 = dao.getCompiledQuery(true)(BoundedSeq100(Seq(id)))
      val query2 = dao2.getCompiledQuery(false)(BoundedSeq100(Seq(id)))
      query1.result.statements.head mustEqual query2.result.statements.head
    }
  }
  "1 additional param" should {
    "handle 1 extra param" in new TestScope {
      val ids = (1 to 99).map(DbUUID(_)) :+ id
      val query = dao.getAppliedCompiledQueryInWithName(ids, "larry")
      val expectedQuery = """select "id", "name" from "persistedslick" where ("id" in (""" +
        questions +
        """)) and ("name" = ?)"""
      query.result.statements.head mustEqual expectedQuery
    }
  }

  trait TestScope
    extends SlickScope with PersistedSlick.Provider {
    val createQuery: DBIOAction[Int, NoStream, Effect] = sqlu"""
      CREATE TABLE `persistedslick` (
       `id` binary(16) NOT NULL,
       `name` varchar(255) NOT NULL,
        PRIMARY KEY (`id`)
      ) ENGINE=InnoDB
    """
    implicit val logger = new NoopBackend {}
    val id = DbUUID.randomDbUUID
    val id2 = DbUUID.randomDbUUID
    scala.concurrent.Await.result(db.run(createQuery.transactionally), Duration(20, SECONDS))
    val dao = new PersistedDao(
      db,
      persistedSlick
    )
    val dao2 = new PersistedDao(
      db,
      persistedSlick
    )
    Await.result(dao.createFuture(PersistedSlick(id, "larry")), Duration(6, TimeUnit.SECONDS))
    Await.result(dao.createFuture(PersistedSlick(id2, "barry")), Duration(6, TimeUnit.SECONDS))
    val questions = (1 to 100).map {_ =>
      "?"
    }.mkString(",")
  }
}

case class PersistedSlick(
  override val id: DbUUID,
  name: String
) extends IdModel[DbUUID] with JdbcTypeImplicits.h2JdbcTypeImplicits.DbImplicits

class PersistedSlickTable(tag: Tag)
  extends H2DAOTable[PersistedSlick, DbUUID](tag, "persistedslick") {

  override def id: Rep[DbUUID] = column[DbUUID]("id")
  def name: slick.lifted.Rep[String] = column[String]("name")

  override def * = ( // scalastyle:ignore
    id,
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
  override protected val db: DBWithLogging,
  private val persistedSlick: TableQuery[PersistedSlickTable]
) extends H2DAO[PersistedSlickTable, PersistedSlick, DbUUID]
  with DAOUUIDQuery[PersistedSlickTable, PersistedSlick, H2Profile]
  with NoopBackend
  with InSeqDbUUIDImplicits {
  override def validateCreate(
    input: PersistedSlick
  )(implicit ec: ExecutionContext): DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read] = {
    DBIO.successful(FormValidatorMessageSeq())
  }

  override def validateUpdate(
    input: PersistedSlick,
    original: PersistedSlick
  )(implicit ec: ExecutionContext): DBIOAction[FormValidatorMessageSeq, profile.api.NoStream, Effect.Read] = {
    DBIO.successful(FormValidatorMessageSeq())
  }

  override def nameSingle: String = "meow"

  override protected val slickQuery: TableQuery[PersistedSlickTable] = persistedSlick

  def queryToCompileSurround(values: Rep[BoundedSeq100[DbUUID]]): Query[PersistedSlickTable, PersistedSlick, Seq] = {
    readQuery
      .filter(_.id.inSeqDbUUID100(values))
      .filter(q => Option(true).bind)
  }

  val compiledQuerySurround: CompiledFunction[Rep[BoundedSeq100[DbUUID]] =>
    Query[PersistedSlickTable, PersistedSlick, Seq],
    Rep[BoundedSeq100[DbUUID]],
    BoundedSeq100[DbUUID],
    Query[PersistedSlickTable, PersistedSlick, Seq],
    Seq[PersistedSlick]] = {
    Compiled(queryToCompileSurround _)
  }

  def queryToCompile(values: Rep[BoundedSeq100[DbUUID]]): Query[PersistedSlickTable, PersistedSlick, Seq] = {
    persistedSlick
      .filter(_.id.inSeqDbUUID100(values))
  }

  val compiledQuery: CompiledFunction[Rep[BoundedSeq100[DbUUID]] => Query[PersistedSlickTable, PersistedSlick, Seq],
      Rep[BoundedSeq100[DbUUID]],
      BoundedSeq100[DbUUID],
      Query[PersistedSlickTable, PersistedSlick, Seq],
      Seq[PersistedSlick]] = {
    Compiled(queryToCompile _)
  }

  def getAppliedCompiledQueryIn(input: Seq[DbUUID]): AppliedCompiledFunction[_, _, Seq[PersistedSlick]] = {
    getCompiledQuerySeq(readQuery, t => t.id, "expandingInput", PersistedDao, input)
  }

  def getAppliedCompiledQueryInWithName(
    ids: Seq[DbUUID],
    name: String
  ): AppliedCompiledFunction[_, _, Seq[PersistedSlick]] = {
    getCompiledQuerySeq(
      readQuery, t => t.id,
      "expandingInput",
      PersistedDao,
      ids,
      (t: PersistedSlickTable, a: Rep[String]) => t.name === a,
      name
    )
  }


  def getCompiledQuery(shouldCall: Boolean): CompiledFunction[_, _, BoundedSeq100[DbUUID], _, Seq[PersistedSlick]] = {
    PersistedDao.getOrInitCompiledQuery("singleInput"){
      require(shouldCall)
      Compiled(queryToCompile _)
    }
  }

  override protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  override protected def addCreateTransaction(id: DbUUID, input: PersistedSlick): Unit = {}

  override protected def addUpdateTransaction(id: DbUUID, input: PersistedSlick, original: PersistedSlick): Unit = {}

  override protected def addDeleteTransaction(id: DbUUID, original: PersistedSlick): Unit = {}
}
object PersistedDao extends SlickCompiledFunctionSingleton
