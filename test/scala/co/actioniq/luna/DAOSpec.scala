package co.actioniq.luna

import java.util.UUID
import java.util.concurrent.TimeUnit

import co.actioniq.luna.dao.{DAOException, DbLongOptId, DbUUID, FormValidatorExceptions, FormValidatorMessageSeq}
import co.actioniq.luna.dao.CoolH2Profile.api._
import co.actioniq.luna.logging.{NoopBackend, TransactionAction, TransactionLogger}
import co.actioniq.luna.example.{FilterLarry, LoggingModel, Player, PlayerDAO, PlayerTable, Team, TeamDAO, TeamTable}
import org.junit.runner.RunWith
import org.mockito.Mockito.{times, verify}
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import slick.jdbc.{GetResult, PositionedParameters, SetParameter}
import slick.util.SlickMDCContext.Implicits.defaultContext
import org.slf4j.MDC

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

@RunWith(classOf[JUnitRunner])
class DAOSpec extends Specification with Mockito {
  "DbUUID" should {
    "handle conversions" in new TestScope with NoopLoggerProvider {
      val randomUuid = UUID.randomUUID().toString
      val test = DbUUID(randomUuid)
      test.toString mustEqual randomUuid
      implicit val getSupplierResult = GetResult(r => r.nextBytes())
      val hexQ = sql"select CAST(REPLACE('#$randomUuid', '-','') as binary)".as[(Array[Byte])]
      val result = awaitResult(db.run(hexQ))
      result.head mustEqual test.binValue
    }
  }


  "MDC Data" should {
    "be maintained in DAO future" in new TestScope with NoopLoggerProvider {
      args.execute(threadsNb = 8)
      val mdcKey = "larry"
      val mdcVal = "mdc wizzard"
      MDC.put(mdcKey, mdcVal)
      val rows = playerDao.readFuture().map { results =>
        MDC.get(mdcKey) mustEqual mdcVal
        results
      }.map { results =>
        MDC.get(mdcKey) mustEqual mdcVal
        results
      }.flatMap { results =>
        MDC.get(mdcKey) mustEqual mdcVal
        Future.successful(results)
      }
      awaitResult(rows)
      MDC.get(mdcKey) mustEqual mdcVal
    }
  }

  "DbLongOptId DAO" should {
    "generate id queries" in new TestScope with NoopLoggerProvider {
      val team = awaitResult(teamDao.readByIdFuture(DbLongOptId(1)))
      team.get.name mustEqual "mets"
      val teams = awaitResult(teamDao.readByIdFuture(Set(DbLongOptId(1), DbLongOptId(2))))
      teams.size mustEqual 2
      teams.head.name mustEqual "mets"
      teams.tail.head.name mustEqual "astros"
    }
    "create and return id" in new TestScope with NoopLoggerProvider {
      val teamToInsert = Team(DbLongOptId(None), "Yanks")
      val id = awaitResult(teamDao.createFuture(teamToInsert))
      id.get mustEqual 4L
    }
    "create and return multiple ids" in new TestScope with NoopLoggerProvider {
      val yanks = Team(DbLongOptId(None), "Yanks")
      val dodgers = Team(DbLongOptId(None), "Dodgers")
      val ids = awaitResult(teamDao.createFuture(Seq(yanks, dodgers)))
      ids must contain(DbLongOptId(4L), DbLongOptId(5L))
    }
    "updateFuture by id" in new TestScope with NoopLoggerProvider {
      val team = awaitResult(teamDao.readByIdFuture(DbLongOptId(1))).get
      val updateFutureAndReadFuture = teamDao.updateAndReadFuture(team.copy(name = "Red Sox"))
      val newTeam = awaitResult(updateFutureAndReadFuture)
      newTeam.id.get mustEqual 1L
      newTeam.name mustEqual "Red Sox"
    }
    "delete by id" in new TestScope with NoopLoggerProvider {
      awaitResult(teamDao.deleteFuture(DbLongOptId(1)))
      awaitResult(teamDao.readByIdFuture(DbLongOptId(1))) must beNone
      awaitResult(teamDao.readByIdFuture(DbLongOptId(2))) must beSome
    }
    "read ids" in new TestScope with NoopLoggerProvider {
      val ids = awaitResult(teamDao.readIdsFuture())
      ids.size mustEqual 3
      ids must containTheSameElementsAs(Seq(DbLongOptId(1), DbLongOptId(2), DbLongOptId(3)))
    }
    "read ids with filter" in new TestScope with NoopLoggerProvider {
      val ids = awaitResult(teamDao.readIdsFuture(q => q.filter(_.name === "mets")))
      ids.size mustEqual 1
      ids must containTheSameElementsAs(Seq(DbLongOptId(1)))
    }
  }

  "DbUUID DAO" should {
    "generate id queries" in new TestScope with NoopLoggerProvider {
      val player = awaitResult(playerDao.readByIdFuture(larryId))
      player.get.name mustEqual "larry"
      val players = awaitResult(playerDao.readByIdFuture(Set(larryId, harryId)))
      players.size mustEqual 2
      players.map(_.name) must contain("harry", "larry")
    }
    "create and return id" in new TestScope with NoopLoggerProvider {
      val playerId = DbUUID.randomDbUUID
      val playerToInsert = Player(playerId, 3L, "Mary")
      val id = awaitResult(playerDao.createFuture(playerToInsert))
      id mustEqual playerId
    }
    "create and return multiple ids" in new TestScope with NoopLoggerProvider {
      val maryId = DbUUID.randomDbUUID
      val zarryId = DbUUID.randomDbUUID
      val marry = Player(maryId, 3L, "Mary")
      val zarry = Player(zarryId, 2L, "Zarry")
      val ids = awaitResult(playerDao.createFuture(Seq(marry, zarry)))
      ids must contain(maryId, zarryId)
    }
    "updateFuture by id" in new TestScope with NoopLoggerProvider {
      val player = awaitResult(playerDao.readByIdFuture(larryId)).get
      val updateFutureAndReadFuture = playerDao.updateAndReadFuture(player.copy(name = "Mary"))
      val newPlayer = awaitResult(updateFutureAndReadFuture)
      newPlayer.id mustEqual larryId
      newPlayer.name mustEqual "Mary"
      newPlayer.teamId mustEqual 1L
    }

    "updateFuture ignore field" in new TestScope with NoopLoggerProvider {
      val player = awaitResult(playerDao.readByIdFuture(larryId)).get
      val ignoreTeamUpdate = playerDao.updateAndReadFuture(player.copy(teamId = 2L, name = "Harp"))
      val ignoreNewPlayer = awaitResult(ignoreTeamUpdate)
      ignoreNewPlayer.id mustEqual larryId
      ignoreNewPlayer.name mustEqual "Harp"
      ignoreNewPlayer.teamId mustEqual 1L
    }

    "update two field" in new TestScope with NoopLoggerProvider {
      val player = awaitResult(playerDao.readByIdFuture(larryId)).get
      val updateFuture = playerDao.updateFieldFuture[String, Long](
        larryId,
        row => (row.name, row.teamId),
        ("booo", 2L),
        (nameAndTeam, original) => DBIO.successful(FormValidatorMessageSeq(Seq())),
        Some(player)
      )
      awaitResult(updateFuture)
      val newPlayer = awaitResult(playerDao.readByIdRequiredFuture(larryId))
      newPlayer.id mustEqual larryId
      newPlayer.name mustEqual "booo"
      newPlayer.teamId mustEqual 2L
    }
    "update two field and validate" in new TestScope with NoopLoggerProvider {
      val player = awaitResult(playerDao.readByIdFuture(larryId)).get
      val updateFuture = playerDao.updateFieldFuture[String, Long](
        larryId,
        row => (row.name, row.teamId),
        ("booo", 2L),
        (nameAndTeam, original) => {
          val errors = FormValidatorMessageSeq()
          errors.assert(nameAndTeam._2 != 2L, "Cannot be in team 2")
          DBIO.successful(errors)
        },
        Some(player)
      )
      awaitResult(updateFuture) must throwA[FormValidatorExceptions]
    }
    "delete by id" in new TestScope with NoopLoggerProvider {
      awaitResult(playerDao.deleteFuture(larryId))
      awaitResult(playerDao.readByIdFuture(larryId)) must beNone
      awaitResult(playerDao.readByIdFuture(harryId)) must beSome
    }
    "read ids" in new TestScope with NoopLoggerProvider {
      val ids = awaitResult(playerDao.readIdsFuture())
      ids.size mustEqual 3
      ids must containTheSameElementsAs(Seq(larryId, harryId, barryId))
    }
    "read ids with filter" in new TestScope with NoopLoggerProvider {
      val ids = awaitResult(playerDao.readIdsFuture(q => q.filter(_.name === "larry")))
      ids.size mustEqual 1
      ids must containTheSameElementsAs(Seq(larryId))
    }
    "read ids only query id" in new TestScope with NoopLoggerProvider {
      val query = playerDao.queryForIds()
      query mustEqual """select "id" from "player""""
    }
  }
  "default filters with joins" should {
    "handle a join monad" in new TestScope with NoopLoggerProvider  {
      playerDao.queryJoinWithMonad() mustEqual
        s"""select x2."id", x2."team_id", x2."name", x3."id", x3."name" from
           | "player" x2, "team" x3 where x3."id" = x2."team_id"""".stripMargin.replaceAll("\n", "")
    }
    "handle a join explicit" in new TestScope with NoopLoggerProvider  {
      playerDao.queryJoinExplicit() mustEqual
        s"""select x2."id", x2."team_id", x2."name", x3."id", x3."name" from "player" x2,
           | "team" x3 where x3."id" = x2."team_id"""".stripMargin.replaceAll("\n", "")
    }
    "handle a join explicit with default filter" in new TestScope with NoopLoggerProvider  {
      larryPlayerDao.queryJoinExplicit() mustEqual
        s"""select x2."id", x2."team_id", x2."name", x3."id", x3."name" from "player" x2, "team" x3 where
           | (x2."name" = 'larry') and (x3."id" = x2."team_id")""".stripMargin.replaceAll("\n", "")
    }
    "handle a join explicit with two default filters" in new TestScope with NoopLoggerProvider  {
      doubleLarryPlayerDao.queryJoinExplicit() mustEqual
        s"""select x2."id", x2."team_id", x2."name", x3."id", x3."name" from "player" x2, "team" x3 where
           | ((x2."name" = 'larry') and (x3."name" = 'larry')) and (x3."id" = x2."team_id")"""
          .stripMargin.replaceAll("\n", "")
    }
    "handle a left join explicit" in new TestScope with NoopLoggerProvider  {
      playerDao.queryLeftJoinExplicit() mustEqual
        s"""select x2."id", x2."team_id", x2."name", x3."id", x3."id", x3."name" from "player" x2 left outer join
           | "team" x3 on (x3."id" = x2."team_id") and ? where ?""".stripMargin.replaceAll("\n", "")
    }
    "handle a left join explicit with two default filters" in new TestScope with NoopLoggerProvider  {
      doubleLarryPlayerDao.queryLeftJoinExplicit() mustEqual
        s"""select x2."id", x2."team_id", x2."name", x3."name", x3."id", x3."name" from "player" x2 left outer
           | join "team" x3 on (x3."id" = x2."team_id") and (x3."name" = 'larry')
           | where x2."name" = 'larry'""".stripMargin.replaceAll("\n", "")
    }
  }

  "joins" should {
    "handle a join without default filters" in new TestScope with NoopLoggerProvider {
      playerDao.innerJoinQuery() mustEqual
        s"""select x2."id", x2."team_id", x2."name", x3."id", x3."name" from "player" x2, "team" x3
           | where x3."id" = x2."team_id"""".stripMargin.replaceAll("\n", "")
    }
    "handle a join with default filter" in new TestScope with NoopLoggerProvider {
      larryPlayerDao.innerJoinQuery() mustEqual
        s"""select x2."id", x2."team_id", x2."name", x3."id", x3."name" from "player" x2, "team" x3
           | where (x2."name" = 'larry') and (x3."id" = x2."team_id")""".stripMargin.replaceAll("\n", "")
    }
    "handle a join with two default filters" in new TestScope with NoopLoggerProvider {
      doubleLarryPlayerDao.innerJoinQuery() mustEqual
        s"""select x2."id", x2."team_id", x2."name", x3."id", x3."name" from "player" x2, "team" x3 where
           | ((x2."name" = 'larry') and (x3."name" = 'larry')) and (x3."id" = x2."team_id")"""
          .stripMargin.replaceAll("\n", "")
    }
    "handle a left join without default filters" in new TestScope with NoopLoggerProvider {
      playerDao.leftJoinQuery() mustEqual
        s"""select x2."id", x2."team_id", x2."name", x3."id", x3."id", x3."name" from "player" x2 left
           | outer join "team" x3 on x3."id" = x2."team_id"""".stripMargin.replaceAll("\n", "")
    }
    "handle a left join with default filter" in new TestScope with NoopLoggerProvider {
      larryPlayerDao.leftJoinQuery() mustEqual
        s"""select x2."id", x2."team_id", x2."name", x3."id", x3."id", x3."name" from "player" x2 left outer join
           | "team" x3 on x3."id" = x2."team_id" where x2."name" = 'larry'""".stripMargin.replaceAll("\n", "")
    }
    "handle a left join with two default filters" in new TestScope with NoopLoggerProvider {
      doubleLarryPlayerDao.leftJoinQuery() mustEqual
        s"""select x2."id", x2."team_id", x2."name", x3."name", x3."id", x3."name" from "player" x2 left outer join
           | "team" x3 on (x3."id" = x2."team_id") and (x3."name" = 'larry')
           | where x2."name" = 'larry'""".stripMargin.replaceAll("\n", "")
    }
    "do a join" in new TestScope with NoopLoggerProvider {
      val players = awaitResult(playerDao.innerJoin())
      val larryAndTeam = players.filter(_._1.id == larryId)
      larryAndTeam.size mustEqual 1
      larryAndTeam.head._1.name mustEqual "larry"
      larryAndTeam.head._2.name mustEqual "mets"
      players.count(_._1.id == barryId) mustEqual 0
    }
    "do a left join" in new TestScope with NoopLoggerProvider {
      val players = awaitResult(playerDao.leftJoin())
      val larryAndTeam = players.filter(_._1.id == larryId)
      larryAndTeam.size mustEqual 1
      larryAndTeam.head._1.name mustEqual "larry"
      larryAndTeam.head._2.get.name mustEqual "mets"
      val barryAndTeam = players.filter(_._1.id == barryId)
      barryAndTeam.size mustEqual 1
      barryAndTeam.head._1.name mustEqual "barry"
      barryAndTeam.head._2.isDefined must beFalse
    }
  }
  "dao validator" should {
    "validate creates" in new TestScope with NoopLoggerProvider {
      val passed = for {
        id <- teamDao.createFuture(Team(id = DbLongOptId(None), name = "Marlins"))
        id2 <- teamDao.createFuture(Team(id = DbLongOptId(None), name = "Giants"))
      } yield (id.get, id2.get)
      awaitResult(passed) mustEqual ((4, 5))
      val failed = teamDao.createFuture(Team(id = DbLongOptId(None), name = "mets"))
      awaitResult(failed) must throwA(new DAOException("Name should be unique"))
    }
    "validate updateFutures" in new TestScope with NoopLoggerProvider {
      awaitResult(teamDao.createFuture(Team(id=DbLongOptId(None), name="Marlins")))
      val failed = teamDao.updateFuture(Team(id = DbLongOptId(1), name = "Harry"))
      awaitResult(failed) must throwA(new DAOException("Name should not be Harry"))
    }
  }
  "log a create updateFuture delete" in new TestScope with MockLoggerProvider {
    val input = Player(DbUUID.randomDbUUID, 1, "Warry")
    var createModel: Option[LoggingModel] = None
    var updateFutureModel: Option[LoggingModel] = None
    var deleteModel: Option[LoggingModel] = None
    var flushCount = 0
      tl.write(any[LoggingModel]) answers(input => {
        val model = input.asInstanceOf[LoggingModel]
        if (model.action == TransactionAction.create){
          createModel = Some(model)
        } else if (model.action == TransactionAction.update){
          updateFutureModel = Some(model)
        } else if (model.action == TransactionAction.delete){
          deleteModel = Some(model)
        }
      })
      tl.flush() answers (v =>{
        if (flushCount == 0) {
          createModel must beSome
          createModel.get.id mustEqual input.id
          updateFutureModel must beNone
          createModel = None
          flushCount = flushCount + 1
        } else if (flushCount == 1) {
          createModel must beNone
          updateFutureModel must beSome
          updateFutureModel.get.id mustEqual input.id
          updateFutureModel.get.name mustEqual "Zarry"
          updateFutureModel = None
          flushCount = flushCount + 1
        }
        Unit
      })
    val createAndReadFuture = awaitResult(playerDao.createAndReadFuture(input))
    awaitResult(playerDao.updateFuture(input.copy(name="Zarry")))
    awaitResult(playerDao.deleteFuture(createAndReadFuture.id))
    createModel must beNone
    updateFutureModel must beNone
    deleteModel.get.id mustEqual input.id
    verify(tl, times(3)).write(any[LoggingModel])
    verify(tl, times(3)).flush()
  }

  "log two actions bundled" in new TestScope with MockLoggerProvider {
    val input = Player(DbUUID.randomDbUUID, 1, "Warry")
    var createModel: Option[LoggingModel] = None
    var updateFutureModel: Option[LoggingModel] = None
    var flushCount = 0
    tl.write(any[LoggingModel]) answers(input => {
      val model = input.asInstanceOf[LoggingModel]
      if (model.action == TransactionAction.create){
        createModel = Some(model)
      } else if (model.action == TransactionAction.update) {
        updateFutureModel = Some(model)
      }
    })
    tl.flush() answers (v =>{
      if (flushCount == 0) {
        createModel must beSome
        createModel.get.id mustEqual input.id
        updateFutureModel.get.id mustEqual input.id
        updateFutureModel.get.name mustEqual "Zarry"
        createModel = None
        updateFutureModel = None
        flushCount = flushCount + 1
      }
      Unit
    })
    awaitResult(playerDao.createAndUpdate(input))
    createModel must beNone
    updateFutureModel must beNone
    verify(tl, times(2)).write(any[LoggingModel])
    verify(tl, times(1)).flush()
  }


  trait LoggerProvider {
    implicit val tl = transactionLogger
    def transactionLogger: TransactionLogger
  }

  trait MockLoggerProvider extends LoggerProvider {
    def transactionLogger: TransactionLogger = mock[TransactionLogger]
  }

  trait NoopLoggerProvider extends LoggerProvider {
    def transactionLogger: TransactionLogger = new NoopBackend {}
  }

  trait TestScope extends SlickScope with Team.Provider with Player.Provider with LoggerProvider {
    val teamDao = new TeamDAO(
      db,
      teamSlick
    )
    val playerDao = new PlayerDAO(
      db,
      playerSlick,
      teamDao
    )
    val larryPlayerDao = new PlayerDAO(
      db,
      playerSlick,
      teamDao
    ) with FilterLarry[PlayerTable, Player, DbUUID]
    val larryTeamDao = new TeamDAO(
      db,
      teamSlick
    ) with FilterLarry[TeamTable, Team, DbLongOptId]
    val doubleLarryPlayerDao = new PlayerDAO(
      db,
      playerSlick,
      larryTeamDao
    ) with FilterLarry[PlayerTable, Player, DbUUID]

    implicit object SetUUID extends SetParameter[DbUUID] {
      def apply(v: DbUUID, pp: PositionedParameters): Unit = {
        pp.setBytes(v.binValue)
      }
    }

    protected val sqlMode = sqlu"SET MODE MySQL"
    protected val createTeam = sqlu"""
      CREATE TABLE `team` (
       `id` bigint auto_increment,
       `name` varchar(255) NOT NULL,
        PRIMARY KEY (`id`)
      )
    """
    protected val mets = sqlu"""
      insert into team(name) values ('mets')
    """
    protected val astros = sqlu"""
      insert into team(name) values ('astros')
    """
    protected val braves = sqlu"""
      insert into team(name) values ('braves')
    """
    protected val createPlayer = sqlu"""
      CREATE TABLE `player` (
       `id` binary(16) NOT NULL,
       `team_id` bigint not null,
       `name` varchar(255) NOT NULL,
        PRIMARY KEY (`id`)
      )
    """
    protected val larryId = DbUUID.randomDbUUID
    protected val larry = sqlu"""
      insert into player
      (id, team_id, name)
      values($larryId, 1, 'larry')
    """
    protected val harryId = DbUUID.randomDbUUID
    protected val harry = sqlu"""
      insert into player
      (id, team_id, name)
      values($harryId, 1, 'harry')
    """
    protected val barryId = DbUUID.randomDbUUID
    protected val barry = sqlu"""
      insert into player
      (id, team_id, name)
      values($barryId, 20, 'barry')
    """
    awaitResult(
      db.run(
        DBIO.seq(
          sqlMode,
          createTeam,
          createPlayer,
          mets,
          astros,
          braves,
          larry,
          harry,
          barry
        )
      )
    )

    def awaitResult[R](future: Future[R]): R = Await.result(future, Duration(6, TimeUnit.SECONDS))
  }
}
