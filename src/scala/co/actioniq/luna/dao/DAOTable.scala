package co.actioniq.luna.dao


import slick.jdbc.{H2Profile, JdbcProfile, MySQLProfile, PostgresProfile}
import slick.lifted.Tag


/**
  * A table that has an IDType for an ID
 *
  * @tparam I IDType
  */
trait IdTable[I <: IdType]{
  def id: slick.lifted.Rep[I]
}

object DAOTable {
  type Table[V <: IdModel[I], I <: IdType, P <: JdbcProfile] = P#Table[V] with IdTable[I]
}

abstract class MySQLDAOTable[V <: IdModel[I], I <: IdType](
  tag: Tag,
  tableName: String,
  schemaName: Option[String] = None
) extends MySQLProfile.Table[V](tag, schemaName, tableName)
  with IdTable[I]
  with JdbcTypeImplicits.mySQLJdbcTypeImplicits.DbImplicits {
  self: DAOTable.Table[V, I, MySQLProfile] =>
}

abstract class PostgresDAOTable[V <: IdModel[I], I <: IdType](
  tag: Tag,
  tableName: String,
  schemaName: Option[String] = None
) extends PostgresProfile.Table[V](tag, schemaName, tableName)
  with IdTable[I]
  with JdbcTypeImplicits.postgresJdbcTypeImplicits.DbImplicits {
  self: DAOTable.Table[V, I, PostgresProfile] =>
}

abstract class H2DAOTable[V <: IdModel[I], I <: IdType](
  tag: Tag,
  tableName: String,
  schemaName: Option[String] = None
) extends H2Profile.Table[V](tag, schemaName, tableName)
  with IdTable[I]
  with JdbcTypeImplicits.h2JdbcTypeImplicits.DbImplicits {
  self: DAOTable.Table[V, I, H2Profile] =>
}

class DAODynamicProfileTable[P <: JdbcProfile] (
  val profile: P,
  val implicits: JdbcTypeImplicits[P]
) {
  abstract class DAOTable[V <: IdModel[I], I <: IdType](
    tag: Tag,
    tableName: String,
    schemaName: Option[String] = None
  ) extends profile.Table[V](tag, schemaName, tableName)
    with IdTable[I]
    with implicits.DbImplicits
}
