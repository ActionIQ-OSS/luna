package co.actioniq.luna.dao

import java.sql.{PreparedStatement, ResultSet}

import slick.jdbc.{H2Profile, JdbcProfile, MySQLProfile, PostgresProfile}

class JdbcTypeImplicits[P <: JdbcProfile](val profile: P) {
  trait DbImplicits extends JdbcTypes[P] {
    override def uuidJdbcType: profile.DriverJdbcType[DbUUID] = {
      new profile.DriverJdbcType[DbUUID]() {
        def sqlType: Int = java.sql.Types.BINARY
        def setValue(v: DbUUID, p: PreparedStatement, idx: Int): Unit = p.setBytes(idx, v.binValue)
        def getValue(r: ResultSet, idx: Int): DbUUID = {
          val value = r.getBytes(idx)
          if (value == null) { // scalastyle:ignore
            null // scalastyle:ignore
          } else {
            DbUUID(r.getBytes(idx))
          }
        }
        def updateValue(v: DbUUID, r: ResultSet, idx: Int): Unit = r.updateBytes(idx, v.binValue)
        override def hasLiteralForm: Boolean = false
      }
    }

    override def optLongJdbcType: profile.DriverJdbcType[DbLongOptId] = {
      new profile.DriverJdbcType[DbLongOptId]() {
        def sqlType: Int = java.sql.Types.BIGINT
        def setValue(v: DbLongOptId, p: PreparedStatement, idx: Int): Unit = v.value match {
          case None => p.setNull(idx, sqlType)
          case Some(s) => p.setLong(idx, s)
        }
        def getValue(r: ResultSet, idx: Int): DbLongOptId = DbLongOptId(r.getLong(idx))
        def updateValue(v: DbLongOptId, r: ResultSet, idx: Int): Unit = v.value match {
          case None => r.updateNull(idx)
          case Some(s) => r.updateLong(idx, s)
        }
        override def hasLiteralForm: Boolean = false
      }
    }
  }
}

object JdbcTypeImplicits {
  val h2JdbcTypeImplicits = new JdbcTypeImplicits[DAOH2Profile](DAOH2Profile)
  val mySQLJdbcTypeImplicits = new JdbcTypeImplicits[DAOMySQLProfile](DAOMySQLProfile)
  val postgresJdbcTypeImplicits = new JdbcTypeImplicits[PostgresProfile](PostgresProfile)
}

trait JdbcTypes[P <: JdbcProfile]{
  def uuidJdbcType: P#DriverJdbcType[DbUUID]
  def optLongJdbcType: P#DriverJdbcType[DbLongOptId]
  protected implicit val dbUuidJdbcType = uuidJdbcType
  protected implicit val dbOptLongJdbcType = optLongJdbcType
}


