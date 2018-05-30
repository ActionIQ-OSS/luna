package co.actioniq.luna.compiled

import java.sql.{PreparedStatement, ResultSet}

import slick.jdbc.JdbcProfile

/**
  * Describe to slick how to interact with a placeholder
  */
class PlaceholderJdbcType(val profile: JdbcProfile) {
  class PlaceholderJdbc extends profile.DriverJdbcType[Placeholder] {
    override def hasLiteralForm: Boolean = true

    def sqlType: Int = java.sql.Types.OTHER

    def setValue(v: Placeholder, p: PreparedStatement, idx: Int): Unit = {}

    def getValue(r: ResultSet, idx: Int): Placeholder = new Placeholder()

    def updateValue(v: Placeholder, r: ResultSet, idx: Int): Unit = {}

    override def valueToSQLLiteral(value: Placeholder): String = {
      if (value eq null) { //scalastyle:ignore
        "NULL"
      } else {
        ""
      }
    }
  }
}
