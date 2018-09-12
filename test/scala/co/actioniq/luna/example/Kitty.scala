package co.actioniq.luna.example

import co.actioniq.luna.dao.{DbUUID, H2DAOTable, H2IdModel}
import slick.jdbc.H2Profile.api._

case class Kitty(
  override val id: DbUUID,
  name: String,
  color: String
) extends H2IdModel[DbUUID]

class KittyTable(tag: Tag)
  extends H2DAOTable[Kitty, DbUUID](tag, "kitty") {

  override def id: Rep[DbUUID] = column[DbUUID]("id")
  def name: Rep[String] = column[String]("name")
  def color: Rep[String] = column[String]("color")
  def deletedAt: Rep[Option[Long]] = column[Option[Long]]("deleted_at")

  override def * = ( // scalastyle:ignore
    id,
    name,
    color
  ) <> (Kitty.tupled, Kitty.unapply)
}

object Kitty {
  trait Provider {
    private val creator = (tag: Tag) => new KittyTable(tag)
    val kittySlick = new TableQuery[KittyTable](creator)
  }
  def tupled = (Kitty.apply _).tupled // scalastyle:ignore
}
