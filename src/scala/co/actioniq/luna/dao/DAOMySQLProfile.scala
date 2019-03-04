package co.actioniq.luna.dao

import slick.compiler.Phase
import slick.jdbc.MySQLProfile

trait DAOMySQLProfile extends MySQLProfile {
  override lazy val updateCompiler = (compiler + new JdbcCodeGen(_.buildUpdate)).addAfter(new IgnoreUpdateCompiler(), Phase.hoistClientOps)
}

object DAOMySQLProfile extends DAOMySQLProfile {
  import slick.ast._
  final case class RowNum(sym: AnonSymbol, inc: Boolean) extends NullaryNode with SimplyTypedNode {
    type Self = RowNum
    def buildType = ScalaBaseType.longType
    def rebuild = copy()
  }

  final case class RowNumGen(sym: AnonSymbol, init: Long) extends NullaryNode with SimplyTypedNode {
    type Self = RowNumGen
    def buildType = ScalaBaseType.longType
    def rebuild = copy()
  }
}