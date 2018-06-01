package co.actioniq.luna.example

import co.actioniq.luna.dao.{DbLongOptId, H2DAOTable, H2IdModel, IdModel, JdbcTypeImplicits}
import slick.jdbc.H2Profile.api._
import slick.lifted.{Rep, TableQuery, Tag}

case class Team(
  override val id: DbLongOptId,
  name: String
) extends H2IdModel[DbLongOptId]

class TeamTable(tag: Tag)
  extends H2DAOTable[Team, DbLongOptId](tag, "team") with NameTable {

  override def id: Rep[DbLongOptId] = column[DbLongOptId]("id", O.AutoInc)
  override def name: Rep[String] = column[String]("name")

  override def * = ( // scalastyle:ignore
    id,
    name
  ) <> (Team.tupled, Team.unapply)
}

object Team {
  trait Provider {
    private val creator = (tag: Tag) => new TeamTable(tag)
    val teamSlick = new TableQuery[TeamTable](creator)
  }
  def tupled = (Team.apply _).tupled // scalastyle:ignore
}
