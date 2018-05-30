package co.actioniq.luna.compiled

import java.sql.{PreparedStatement, ResultSet}

import co.actioniq.luna.dao.DbUUID
import slick.jdbc.JdbcProfile

import scala.reflect.ClassTag

class SeqDbUUIDJdbcType(val profile: JdbcProfile) {
  abstract class SeqDbUUIDJdbc[T <: BoundedSeq[DbUUID]: ClassTag] extends profile.DriverJdbcType[T] {
    override def hasLiteralForm: Boolean = false

    def sqlType: Int = java.sql.Types.BLOB

    /**
      * This is where the magic happens.  This expands a sequence input into individual items to set parameters.  It
      * will assume 100 max and set the missing parameters to empty / placeholder
      *
      * @param v   sequence of ids
      * @param p   prepared statement
      * @param idx index for parameter
      */
    def setValue(v: T, p: PreparedStatement, idx: Int): Unit = {
      val size = v.seq.length
      for {(x, i) <- v.seq.zipWithIndex} {
        p.setBytes(idx + i, x.binValue)
      }
      for {i <- size until v.limit} {
        p.setBytes(i + idx, "".getBytes)
      }
    }

    def getValue(r: ResultSet, idx: Int): T =
      BoundedSeq(r.getString(idx).split(",").map(DbUUID(_))).asInstanceOf[T]

    def updateValue(
      v: T,
      r: ResultSet,
      idx: Int
    ): Unit = r.updateString(idx, v.seq.map(_.toString).mkString(","))

    override def valueToSQLLiteral(value: T): String = {
      if (value eq null) { //scalastyle:ignore
        "NULL"
      } else {
        value.seq.map(v => s"z'${v.toString}").mkString(",")
      }
    }
  }
  class SeqDbUUIDJdbcTypeBase extends SeqDbUUIDJdbc[BoundedSeq[DbUUID]]
  class SeqDbUUIDJdbcType100 extends SeqDbUUIDJdbc[BoundedSeq100[DbUUID]]
  class SeqDbUUIDJdbcType200 extends SeqDbUUIDJdbc[BoundedSeq200[DbUUID]]
  class SeqDbUUIDJdbcType500 extends SeqDbUUIDJdbc[BoundedSeq500[DbUUID]]
  class SeqDbUUIDJdbcType1000 extends SeqDbUUIDJdbc[BoundedSeq1000[DbUUID]]
}
