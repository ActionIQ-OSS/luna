package co.actioniq.luna.dao

import slick.compiler.Phase
import slick.driver.JdbcProfile
import slick.jdbc.H2Profile

trait DAOH2Profile extends H2Profile { self => JdbcProfile
  override lazy val updateCompiler = (compiler + new JdbcCodeGen(_.buildUpdate)).addAfter(new IgnoreUpdateCompiler(), Phase.hoistClientOps)
}

object DAOH2Profile extends DAOH2Profile