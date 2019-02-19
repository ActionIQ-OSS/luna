package co.actioniq.luna.example

import co.actioniq.luna.dao.{DbUUID, H2DAOTable, H2IdModel, IdModel, JdbcTypeImplicits}
import co.actioniq.luna.dao.CoolH2Profile.api._
import slick.lifted.{Rep, Tag}

case class Player(
  override val id: DbUUID,
  teamId: Long,
  name: String
) extends H2IdModel[DbUUID]

class PlayerTable(tag: Tag)
  extends H2DAOTable[Player, DbUUID](tag, "player") with NameTable {

  override def id: Rep[DbUUID] = column[DbUUID]("id")
  def teamId: Rep[Long] = column[Long]("team_id")
  override def name: Rep[String] = column[String]("name")

  override def * = ( // scalastyle:ignore
    id,
    teamId,
    name
  ) <> (Player.tupled, Player.unapply)
}

object Player {
  trait Provider {
    private val creator = (tag: Tag) => new PlayerTable(tag)
    val playerSlick = new TableQuery[PlayerTable](creator)
  }
  def tupled = (Player.apply _).tupled // scalastyle:ignore
}
