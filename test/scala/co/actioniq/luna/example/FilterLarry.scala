package co.actioniq.luna.example

import co.actioniq.luna.dao.{CoolH2Profile, DefaultFilter, H2DAOTable, IdModel, IdType}
import slick.jdbc.{H2Profile, JdbcProfile}

trait FilterLarry[T <: H2DAOTable[V, I]
  with NameTable, V <: IdModel[I], I <: IdType]
  extends DefaultFilter[T, V, I, CoolH2Profile] {
  protected val profile: JdbcProfile
  protected val name: String = "larry"
  import profile.api._ // scalastyle:ignore

  addDefaultFilter(t => t.name === name)
}
