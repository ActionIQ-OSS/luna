package co.actioniq.luna.example

import co.actioniq.luna.DBWithLogging
import co.actioniq.luna.dao.{CoolH2Profile, DAOUUIDQuery, DbLongOptId, DbUUID, FormValidatorMessageSeq, H2DAO}
import co.actioniq.luna.dao.CoolH2Profile.api._
import co.actioniq.luna.logging.TransactionAction
import slick.dbio.DBIOAction
import slick.lifted.{Rep, TableQuery}
import slick.util.SlickMDCContext

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class PlayerDAO(
  override val db: DBWithLogging,
  override val slickQuery: TableQuery[PlayerTable],
  val teamDao: TeamDAO
) extends H2DAO[PlayerTable, Player, DbUUID]
  with DAOUUIDQuery[PlayerTable, Player, CoolH2Profile] {

  override protected implicit val ec: ExecutionContext = SlickMDCContext.Implicits.defaultContext

  override protected def addCreateTransaction(id: DbUUID, input: Player): Unit =
    db.transactionLogger.write(LoggingModel(TransactionAction.create, id, input.name))

  override protected def addUpdateTransaction(id: DbUUID, input: Player, original: Player): Unit =
    db.transactionLogger.write(LoggingModel(TransactionAction.update, id, input.name))

  override protected def addDeleteTransaction(id: DbUUID, original: Player): Unit = {
    db.transactionLogger.write(LoggingModel(TransactionAction.delete, id, original.name))
  }

  override def nameSingle: String = ???

  override def validateCreate(
    input: Player
  )(implicit ec: ExecutionContext): DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read] = {
    DBIOAction.successful(FormValidatorMessageSeq())
  }

  override def validateUpdate(
    input: Player,
    original: Player
  )(implicit ec: ExecutionContext): DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read] = {
    DBIOAction.successful(FormValidatorMessageSeq())
  }

  def queryJoinWithMonad(): String = {
    val q = for {
      player <- readQuery
      team <- teamDao.readQuery if optLongCompare(team.id) equalsLong player.teamId
    } yield (player, team)
    q.result.statements.head
  }

  def queryJoinExplicit(): String = {
    applyDefaultFilters(
      getSlickQuery
        .join(teamDao.getSlickQuery)
        .on((player, team) => optLongCompare(team.id) equalsLong player.teamId),
      teamDao
    ).result.statements.head
  }

  def queryForIds(): String = {
    idMap(readQuery).result.statements.head
  }

  def queryLeftJoinExplicit(): String = {
    getSlickQuery.joinLeft(teamDao.getSlickQuery)
      .on((player, team) => optLongCompare(team.id).equalsLong(player.teamId) && teamDao.getDefaultFilters(team))
      .filter {
        case (player: PlayerTable, team: Rep[Option[TeamTable]]) =>
          getDefaultFilters(player)
      }.result.statements.head
  }

  def innerJoin(): Future[Seq[(Player, Team)]] = {
    readJoinFuture[TeamTable, Team, DbLongOptId](
      teamDao,
      (player, team) => optLongCompare(team.id) equalsLong player.teamId
    )
  }

  def innerJoinQuery(): String = {
    joinQuery[TeamTable, Team, DbLongOptId](
      teamDao,
      (player, team) => optLongCompare(team.id) equalsLong player.teamId
    ).result.statements.head
  }

  def leftJoinQuery(): String = {
    leftJoinQuery[TeamTable, Team, DbLongOptId](
      teamDao,
      (player, team) => optLongCompare(team.id) equalsLong player.teamId
    ).result.statements.head
  }
  def leftJoin(): Future[Seq[(Player, Option[Team])]] = {
    readLeftJoinFuture[TeamTable, Team, DbLongOptId](
      teamDao,
      (player, team) => optLongCompare(team.id) equalsLong player.teamId
    )
  }
  def createAndUpdate(input: Player): Future[DbUUID] = {
    val actions = for {
      id <- createAction(input)
      update <- updateAction(input.copy(name = "Zarry"), false, Some(input))
    } yield id
    runTransactionFuture(actions)
  }
}
