package co.actioniq.luna.dao

import slick.jdbc.JdbcProfile

/**
  * Default filter is the "lowest" level of the dao traits that simply lets you add a default filter to any query that
  * the DAO runs.  This is useful for customer_id, team_id, etc...
  * @tparam T slick table, extends aiqtable
  * @tparam V case class to store result set rows
  * @tparam I id type (option long and uuid)
  */
trait DefaultFilter[T <: DAOTable.Table[V, I, P], V <: IdModel[I], I <: IdType, P <: JdbcProfile] {
  protected val profile: JdbcProfile
  import profile.api._ // scalastyle:ignore

  protected type QueryWithFilter =
    Query[T, T#TableElementType, Seq]
  // type alias for query
  protected type QueryJoin[A, B] = Query[(T, A), (V, B), Seq]
  protected type QueryJoinTwo[A, B, AA, BB] = Query[((T, A), AA), ((V, B), BB), Seq]
  // type alias for left join query
  protected type QueryLeftJoin[A, B] = Query[(T, Rep[Option[A]]), (V, Option[B]), Seq]
  protected val slickQuery: TableQuery[T]
  private var defaultFilters: List[T => Rep[Boolean]] = List()
  private var defaultOptFilters: List[T => Rep[Option[Boolean]]] = List()

  def getSlickQuery: TableQuery[T] = slickQuery

  protected def addDefaultFilter(filter: T => Rep[Boolean]): Unit = {
    defaultFilters = defaultFilters.::(filter)
  }

  protected def addDefaultOptFilter(filter: T => Rep[Option[Boolean]]): Unit = {
    defaultOptFilters = defaultOptFilters.::(filter)
  }

  private def reduceFilters(t: T) = defaultFilters.reduceLeft((original, next) => inT => original(inT) && next(inT))(t)

  private def reduceOptFilters(t: T) = defaultOptFilters
    .reduceLeft((original, next) => inT => original(inT) && next(inT))(t)

  def getDefaultFilters(t: T): Rep[Option[Boolean]] = {
    (defaultFilters, defaultOptFilters) match {
      case (Nil, Nil) => Option(true).bind
      case (Nil, _) => reduceOptFilters(t)
      case (_, Nil) => reduceFilters(t)
      case (_, _) => reduceFilters(t) && reduceOptFilters(t)
    }
  }

  def ifDefaultFiltersExist[R](exists: () => R, doesNotExist: () => R): R = {
    (defaultFilters, defaultOptFilters) match {
      case (Nil, Nil) => doesNotExist()
      case (_, _) => exists()
    }
  }

  def applyDefaultFilters(query: QueryWithFilter): QueryWithFilter = {
    ifDefaultFiltersExist(
      exists = () => query.filter(row => getDefaultFilters(row)),
      doesNotExist = () => query
    )
  }

  def applyDefaultFilters[A <: DAOTable.Table[B, _, P], B <: IdModel[_]](
    query: QueryJoin[A, B],
    other: DefaultFilter[A, B, _, P]
  ): QueryJoin[A, B] = {
    val left = ifDefaultFiltersExist(
      exists = () => query.filter(row => getDefaultFilters(row._1)),
      doesNotExist = () => query
    )
    other.ifDefaultFiltersExist(
      exists = () => left.filter(row => other.getDefaultFilters(row._2)),
      doesNotExist = () => left
    )
  }

  def applyDefaultFiltersLeftJoin[A <: DAOTable.Table[B, _, P], B <: IdModel[_]](
    query: QueryLeftJoin[A, B]
  ): QueryLeftJoin[A, B] = {
    ifDefaultFiltersExist(
      exists = () => query.filter(row => getDefaultFilters(row._1)),
      doesNotExist = () => query
    )
  }
}






