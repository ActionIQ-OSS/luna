package co.actioniq.luna.dao


import co.actioniq.luna.compiled.{BoundedSeq100, BoundedSeq1000, BoundedSeq200, BoundedSeq500, InSeqDbUUIDImplicits, SlickCompiledFunctionSingleton}
import slick.jdbc.JdbcProfile
import slick.lifted.{AppliedCompiledFunction, CompiledFunction}

import scala.concurrent.ExecutionContext

// scalastyle:off parameter.number
/**
  * Generates the base queries that most DAO classes will need
  * @tparam T slick table, extends aiqtable
  * @tparam V case class to store result set rows
  * @tparam I id type (option long and uuid)
  */
trait DAOQuery[T <: DAOTable.Table[V, I, P], V <: IdModel[I], I <: IdType, P <: JdbcProfile]
  extends DefaultFilter[T, V, I, P] with IdQuery[T, V, I, P]{
  protected val profile: JdbcProfile
  import profile.api._ // scalastyle:ignore

  // Name of object, used for generic error messages like in update "id does not exist for $nameSingle"
  def nameSingle: String

  /**
    * Build an inner join query between two daos
    * @param other other DAO to join to
    * @param on lambda filter function to specify "on" clause for join
    * @param extraQueryOps extra where clause
    * @tparam A type of other slick table
    * @tparam B type of other slick model
    * @tparam C type of other idtype
    * @return query to do join
    */
  def joinQuery[A <: DAOTable.Table[B, C, P], B <: IdModel[C], C <: IdType]
  (
    other: DAOQuery[A, B, C, P],
    on: (T, A) => Rep[Option[Boolean]],
    extraQueryOps: QueryJoin[A, B] => QueryJoin[A, B] = (query: QueryJoin[A, B]) => query
  ):
  QueryJoin[A, B]= {
    extraQueryOps(
      applyDefaultFilters(
        slickQuery.join(other.getSlickQuery).on((mine, theirs) => on(mine, theirs)),
        other
      )
    )
  }

  def joinQueryTwo[A <: DAOTable.Table[B, C, P], B <: IdModel[C], C <: IdType,
  AA <: DAOTable.Table[BB, CC, P], BB <: IdModel[CC], CC <: IdType]
  (
    otherFirst: DAOQuery[A, B, C, P],
    onFirst: (T, A) => Rep[Option[Boolean]],
    otherSecond: DAOQuery[AA, BB, CC, P],
    onSecond: (T, A, AA) => Rep[Option[Boolean]],
    extraQueryOps: QueryJoinTwo[A, B, AA, BB] => QueryJoinTwo[A, B, AA, BB]
      = (query: QueryJoinTwo[A, B, AA, BB]) => query
  ):
  QueryJoinTwo[A, B, AA, BB]= {
    val queryWithoutExtra = slickQuery
      .join(otherFirst.getSlickQuery).on((mine, theirs) => onFirst(mine, theirs))
      .join(otherSecond.getSlickQuery).on((mineAndFirst, second) => onSecond(mineAndFirst._1, mineAndFirst._2, second))
      .filter(f =>
        getDefaultFilters(f._1._1) && otherFirst.getDefaultFilters(f._1._2) && otherSecond.getDefaultFilters(f._2)
      )
    extraQueryOps(queryWithoutExtra)
  }

  /**
    * Build a left join query between two daos
    * @param other other DAO to join to
    * @param on lambda filter function to specify "on" clause for join
    * @param extraQueryOps extra where clause
    * @tparam A type of other slick table
    * @tparam B type of other slick model
    * @tparam C type of other idtype
    * @return query to do left join, "other" piece is option to return
    */
  def leftJoinQuery[A <: DAOTable.Table[B, C, P], B <: IdModel[C], C <: IdType]
  (
    other: DAOQuery[A, B, C, P],
    on: (T, A) => Rep[Option[Boolean]],
    extraQueryOps: QueryLeftJoin[A, B] => QueryLeftJoin[A, B] = (query: QueryLeftJoin[A, B]) => query
  ):
  QueryLeftJoin[A, B] = {
    extraQueryOps(
      applyDefaultFiltersLeftJoin(
        slickQuery.joinLeft(other.getSlickQuery).on((mine, theirs) => {
          other.ifDefaultFiltersExist(
            exists = () => on(mine, theirs) && other.getDefaultFilters(theirs),
            doesNotExist = () => on(mine, theirs)
          )
        })
      )
    )
  }

  /**
    * Build a read query.  Most query functions will use this as a basis for building their queries so default
    * filters get used throughout.  The only exception is joins.
    * @return
    */
  def readQuery: QueryWithFilter = {
    applyDefaultFilters(slickQuery)
  }

  /**
    * Build an update query.  Return id of object
    * @param id object id to update
    * @param input new object to store
    * @param ec
    * @return
    */
  def updateQuery(id: I, input: T#TableElementType)(implicit ec: ExecutionContext):
  DBIOAction[I, NoStream, Effect.Write] = {
    idEquals(readQuery, id)
      .update(input)
      .map(rowsAffected => id)
  }

  /**
    * Build a delete query
    * @param id object id to delete
    * @return
    */
  def deleteQuery(id: I): DBIOAction[Int, NoStream, Effect.Write] = {
    idEquals(readQuery, id)
      .delete
  }

  /**
    * Build a read query based on an id
    * @param id object id
    * @return
    */
  def readByIdQuery(id: I): QueryWithFilter = {
    idEquals(readQuery, id)
  }

  /**
    * Build a read query for a set of ids
    * @param id set of ids
    * @return
    */
  def readByIdQuery(id: Set[I]): QueryWithFilter = {
    idInSet(readQuery, id.toSeq)
  }

}

/**
  * Functions for handling queries against the different id types
  * @tparam T slick table, extends aiqtable
  * @tparam V case class to store result set rows
  * @tparam I id type (option long and uuid)
  */
trait IdQuery[T <: DAOTable.Table[V, I, P], V <: IdModel[I], I <: IdType, P <: JdbcProfile]
  extends DefaultFilter[T, V, I, P] {
  protected val profile: JdbcProfile
  import profile.api._ // scalastyle:ignore

  /**
    * Query to filter id = something
    * @param query existing query
    * @param id id to filter on
    * @return
    */
  def idEquals(query: QueryWithFilter, id: I): QueryWithFilter

  /**
    * Query to filter id in (some seq)
    * @param query existing query
    * @param ids ids to filter on
    * @return
    */
  def idInSet(query: QueryWithFilter, ids: Seq[I]): QueryWithFilter

  /**
    * Retrieve ID column from query
    * @return
    */
  def idMap: Query[Rep[I], I, Seq]

  /**
    * Retrieve sequence of ids from sequence of rows
    * @param input sequence of rows
    * @return
    */
  def idsAsSeq(input: Seq[V]): Seq[I]

  /**
    * Create a query for creating objects.  This query depends on the id type since MySQL can only return ids of
    * autoincs
    * @param input new row
    * @param ec
    * @return
    */
  def createQuery(input: V)(implicit ec: ExecutionContext):
  DBIOAction[I, NoStream, Effect.Read with Effect.Write]

  /**
    * Get compiled query with input applied
    * @param originalQuery baseline query to add IN filter to
    * @param fieldFunc lambda to get id field from table
    * @param keyPrefix string keyprefix for storing in singleton
    * @param storage singleton to store compiled queries
    * @param ids ids to filter the field defined in fieldFunc
    * @return applied compiled function
    */
  def getCompiledQuerySeq(
    originalQuery: QueryWithFilter,
    fieldFunc: T => Rep[I],
    keyPrefix: String,
    storage: SlickCompiledFunctionSingleton,
    ids: Seq[I]
  ): AppliedCompiledFunction[_, _, Seq[V]]

  /**
    * Get compiled query with input applied.  Adds extra field for filtering
    * @param originalQuery baseline query to add IN filter to
    * @param fieldFunc lambda to get id field from table
    * @param keyPrefix string keyprefix for storing in singleton
    * @param storage singleton to store compiled queries
    * @param ids to filter the field defined in fieldFunc
    * @param field1 lambda function to apply filter for extra field
    * @param field1Value value to filter extra field on
    * @param aTpe implicit way to query type A
    * @tparam A type of field1
    * @return applied compiled function
    */
  def getCompiledQuerySeq[A](
    originalQuery: QueryWithFilter,
    fieldFunc: T => Rep[I],
    keyPrefix: String,
    storage: SlickCompiledFunctionSingleton,
    ids: Seq[I],
    field1: (T, Rep[A]) => Rep[Boolean],
    field1Value: A
  )(implicit aTpe: P#DriverJdbcType[A]): AppliedCompiledFunction[_, _, Seq[V]]

  /**
    * Get compiled query with input applied.  Adds TWO extra field for filtering
    * @param originalQuery baseline query to add IN filter to
    * @param fieldFunc lambda to get id field from table
    * @param keyPrefix string keyprefix for storing in singleton
    * @param storage singleton to store compiled queries
    * @param ids to filter the field defined in fieldFunc
    * @param field1 lambda function to apply first filter for extra field
    * @param field1Value value to filter first extra field on
    * @param field2 lambda function to apply second filter for extra field
    * @param field2Value value to filter second extra field on
    * @param aTpe implicit way to query type A
    * @param bTpe implicit way to query type B
    * @tparam A type of field1
    * @tparam B type of field2
    * @return applied compiled function
    */
  def getCompiledQuerySeq[A, B](
    originalQuery: QueryWithFilter,
    fieldFunc: T => Rep[I],
    keyPrefix: String,
    storage: SlickCompiledFunctionSingleton,
    ids: Seq[I],
    field1: (T, Rep[A]) => Rep[Boolean],
    field1Value: A,
    field2: (T, Rep[B]) => Rep[Boolean],
    field2Value: B
  )(
    implicit aTpe: P#DriverJdbcType[A],
    bTpe: P#DriverJdbcType[B]
  ): AppliedCompiledFunction[_, _, Seq[V]]
}

/**
  * Implementation of handling queries with option[long] id
  * @tparam T slick table, extends aiqtable
  * @tparam V case class to store result set rows
  */
trait DAOLongIdQuery[T <: DAOTable.Table[V, DbLongOptId, P], V <: IdModel[DbLongOptId], P <: JdbcProfile]
  extends IdQuery[T, V, DbLongOptId, P] with JdbcTypes[P] with InSeqDbUUIDImplicits {
  protected val profile: JdbcProfile
  import profile.api._ // scalastyle:ignore

  /**
    * Filter id equals DbLongOptID
    * @param query existing query
    * @param id id to filter on
    * @return
    */
  def idEquals(query: QueryWithFilter, id: DbLongOptId): QueryWithFilter = {
    query.filter(_.id === id)
  }

  /**
    * Filter id in seq of DbLongOptID
    * @param query existing query
    * @param ids ids to filter on
    * @return
    */
  def idInSet(query: QueryWithFilter, ids: Seq[DbLongOptId]): QueryWithFilter = {
    query.filter(_.id inSet(ids))
  }

  /**
    * Retrieve ID column from query
    * @return
    */
  def idMap: Query[Rep[DbLongOptId], DbLongOptId, Seq] = slickQuery.map(_.id)

  /**
    * Retrieve seq of ids from resultset
    * @param input sequence of rows
    * @return
    */
  def idsAsSeq(input: Seq[V]): Seq[DbLongOptId] = input.map(_.id)

  /**
    * Generate create query
    * @param input new row
    * @param ec
    * @return autoinc id
    */
  def createQuery(input: V)(implicit ec: ExecutionContext):
  DBIOAction[DbLongOptId, NoStream, Effect.Read with Effect.Write] = {
    slickQuery returning slickQuery.map(_.id) += input
  }

  override def getCompiledQuerySeq(
    originalQuery: QueryWithFilter,
    fieldFunc: T => Rep[DbLongOptId],
    keyPrefix: String,
    storage: SlickCompiledFunctionSingleton,
    ids: Seq[DbLongOptId]
  ): AppliedCompiledFunction[_, _, Seq[V]] = {
    if (ids.length <= 100){
      val toCall = storage.getOrInitCompiledQuery(s"${keyPrefix}_100") {
        def funct(
          idValues: Rep[BoundedSeq100[DbLongOptId]]
        ): Query[T, V, Seq] = {
          originalQuery.filter(q => fieldFunc(q).inSeqLongOpt100(idValues))
        }
        Compiled(funct _)
      }
      toCall(BoundedSeq100(ids))
    } else if (ids.length <= 200) {
      val toCall = storage.getOrInitCompiledQuery(s"${keyPrefix}_200") {
        def funct(
          idValues: Rep[BoundedSeq200[DbLongOptId]]
        ): Query[T, V, Seq] = {
          originalQuery.filter(q => fieldFunc(q).inSeqLongOpt200(idValues))
        }
        Compiled(funct _)
      }
      toCall(BoundedSeq200(ids))
    } else if (ids.length <= 500) {
      val toCall = storage.getOrInitCompiledQuery(s"${keyPrefix}_500") {
        def funct(
          idValues: Rep[BoundedSeq500[DbLongOptId]]
        ): Query[T, V, Seq] = {
          originalQuery.filter(q => fieldFunc(q).inSeqLongOpt500(idValues))
        }
        Compiled(funct _)
      }
      toCall(BoundedSeq500(ids))
    } else if (ids.length <= 1000) {
      val toCall = storage.getOrInitCompiledQuery(s"${keyPrefix}_1000") {
        def funct(
          idValues: Rep[BoundedSeq1000[DbLongOptId]]
        ): Query[T, V, Seq] = {
          originalQuery.filter(q => fieldFunc(q).inSeqLongOpt1000(idValues))
        }
        Compiled(funct _)
      }
      toCall(BoundedSeq1000(ids))
    } else {
      throw new Exception("Queries cannot handle more than 1000 items")
    }
  }

  override def getCompiledQuerySeq[A](
    originalQuery: QueryWithFilter,
    fieldFunc: T => Rep[DbLongOptId],
    keyPrefix: String,
    storage: SlickCompiledFunctionSingleton,
    ids: Seq[DbLongOptId],
    field1: (T, Rep[A]) => Rep[Boolean],
    field1Value: A
  )(implicit aTpe: P#DriverJdbcType[A]): AppliedCompiledFunction[_, _, Seq[V]] = {
    if (ids.length <= 100) {
      val toCall = storage.getOrInitCompiledQuery(s"${keyPrefix}_100"){
        def funct(
          idValues: Rep[BoundedSeq100[DbLongOptId]],
          field1Val: Rep[A]
        ): Query[T, V, Seq] = {
          originalQuery.filter(q => fieldFunc(q).inSeqLongOpt100(idValues))
            .filter(q => field1(q, field1Val))
        }
        Compiled(funct _)
      }
      toCall((BoundedSeq100(ids), field1Value))
    } else if (ids.length <= 200){
      val toCall = storage.getOrInitCompiledQuery(s"${keyPrefix}_200") {
        def funct(
          idValues: Rep[BoundedSeq200[DbLongOptId]],
          field1Val: Rep[A]
        ): Query[T, V, Seq] = {
          originalQuery.filter(q => fieldFunc(q).inSeqLongOpt200(idValues))
            .filter(q => field1(q, field1Val))
        }
        Compiled(funct _)
      }
      toCall((BoundedSeq200(ids), field1Value))
    } else if (ids.length <= 500){
      val toCall = storage.getOrInitCompiledQuery(s"${keyPrefix}_500") {
        def funct(
          idValues: Rep[BoundedSeq500[DbLongOptId]],
          field1Val: Rep[A]
        ): Query[T, V, Seq] = {
          originalQuery.filter(q => fieldFunc(q).inSeqLongOpt500(idValues))
            .filter(q => field1(q, field1Val))
        }
        Compiled(funct _)
      }
      toCall((BoundedSeq500(ids), field1Value))
    } else if (ids.length <= 1000){
      val toCall = storage.getOrInitCompiledQuery(s"${keyPrefix}_1000") {
        def funct(
          idValues: Rep[BoundedSeq1000[DbLongOptId]],
          field1Val: Rep[A]
        ): Query[T, V, Seq] = {
          originalQuery.filter(q => fieldFunc(q).inSeqLongOpt1000(idValues))
            .filter(q => field1(q, field1Val))
        }
        Compiled(funct _)
      }
      toCall((BoundedSeq1000(ids), field1Value))
    } else {
      throw new Exception("Queries cannot handle more than 1000 items")
    }
  }

  override def getCompiledQuerySeq[A, B](
    originalQuery: QueryWithFilter,
    fieldFunc: T => Rep[DbLongOptId],
    keyPrefix: String,
    storage: SlickCompiledFunctionSingleton,
    ids: Seq[DbLongOptId],
    field1: (T, Rep[A]) => Rep[Boolean],
    field1Value: A,
    field2: (T, Rep[B]) => Rep[Boolean],
    field2Value: B
  )(
    implicit aTpe: P#DriverJdbcType[A],
    bTpe: P#DriverJdbcType[B]
  ): AppliedCompiledFunction[_, _, Seq[V]] = {
    if (ids.length <= 100) {
      val toCall = storage.getOrInitCompiledQuery(s"${keyPrefix}_100") {
        def funct(
          idValues: Rep[BoundedSeq100[DbLongOptId]],
          field1Val: Rep[A],
          field2Val: Rep[B]
        ): Query[T, V, Seq] = {
          originalQuery.filter(q => fieldFunc(q).inSeqLongOpt100(idValues))
            .filter(q => field1(q, field1Val))
            .filter(q => field2(q, field2Val))
        }
        Compiled(funct _)
      }
      toCall((BoundedSeq100(ids), field1Value, field2Value))
    } else if (ids.length <= 200) {
      val toCall = storage.getOrInitCompiledQuery(s"${keyPrefix}_200") {
        def funct(
          idValues: Rep[BoundedSeq200[DbLongOptId]],
          field1Val: Rep[A],
          field2Val: Rep[B]
        ): Query[T, V, Seq] = {
          originalQuery.filter(q => fieldFunc(q).inSeqLongOpt200(idValues))
            .filter(q => field1(q, field1Val))
            .filter(q => field2(q, field2Val))
        }

        Compiled(funct _)
      }
      toCall((BoundedSeq200(ids), field1Value, field2Value))
    } else if (ids.length <= 500) {
      val toCall = storage.getOrInitCompiledQuery(s"${keyPrefix}_500") {
        def funct(
          idValues: Rep[BoundedSeq500[DbLongOptId]],
          field1Val: Rep[A],
          field2Val: Rep[B]
        ): Query[T, V, Seq] = {
          originalQuery.filter(q => fieldFunc(q).inSeqLongOpt500(idValues))
            .filter(q => field1(q, field1Val))
            .filter(q => field2(q, field2Val))
        }
        Compiled(funct _)
      }
      toCall((BoundedSeq500(ids), field1Value, field2Value))
    } else if (ids.length <= 1000) {
      val toCall = storage.getOrInitCompiledQuery(s"${keyPrefix}_1000") {
        def funct(
          idValues: Rep[BoundedSeq1000[DbLongOptId]],
          field1Val: Rep[A],
          field2Val: Rep[B]
        ): Query[T, V, Seq] = {
          originalQuery.filter(q => fieldFunc(q).inSeqLongOpt1000(idValues))
            .filter(q => field1(q, field1Val))
            .filter(q => field2(q, field2Val))
        }
        Compiled(funct _)
      }
      toCall((BoundedSeq1000(ids), field1Value, field2Value))
    } else {
      throw new Exception("Queries cannot handle more than 1000 items")
    }
  }

}

/**
  * Implementation of handling queries with UUID
  * @tparam T slick table, extends aiqtable
  * @tparam V case class to store result set rows
  */
trait DAOUUIDQuery[T <: DAOTable.Table[V, DbUUID, P], V <: IdModel[DbUUID], P <: JdbcProfile]
  extends IdQuery[T, V, DbUUID, P] with JdbcTypes[P] with InSeqDbUUIDImplicits {
  protected val profile: JdbcProfile
  import profile.api._ // scalastyle:ignore

  import slick.driver.MySQLDriver.DriverJdbcType // scalastyle:ignore

  /**
    * Filter id equals UUID
    * @param query existing query
    * @param id id to filter on
    * @return
    */
  def idEquals(query: QueryWithFilter, id: DbUUID): QueryWithFilter = {
    query.filter(_.id === id)
  }

  /**
    * Filter id in seq of UUID
    * @param query existing query
    * @param ids ids to filter on
    * @return
    */
  def idInSet(query: QueryWithFilter, ids: Seq[DbUUID]): QueryWithFilter = {
    query.filter(_.id inSet(ids))
  }

  /**
    * Retrieve ID column from query
    * @return
    */
  def idMap: Query[Rep[DbUUID], DbUUID, Seq] = slickQuery.map(_.id)

  /**
    * Retrieve seq of ids from resultset
    * @param input sequence of rows
    * @return
    */
  def idsAsSeq(input: Seq[V]): Seq[DbUUID] = input.map(_.id)

  /**
    * Generate create query
    * @param input new row
    * @param ec
    * @return UUID
    */
  def createQuery(input: T#TableElementType)(implicit ec: ExecutionContext):
  DBIOAction[DbUUID, NoStream, Effect.Read with Effect.Write] = {
    for {
      insert <- slickQuery += input
      id <- DBIO.successful(input.id)
    } yield id
  }

  override def getCompiledQuerySeq(
    originalQuery: QueryWithFilter,
    fieldFunc: T => Rep[DbUUID],
    keyPrefix: String,
    storage: SlickCompiledFunctionSingleton,
    ids: Seq[DbUUID]
  ): AppliedCompiledFunction[_, _, Seq[V]] = {
    if (ids.length <= 100){
      getCompiledQuerySeqFunc100(originalQuery, fieldFunc, keyPrefix, storage)(BoundedSeq100(ids))
    } else if (ids.length <= 200) {
      getCompiledQuerySeqFunc200(originalQuery, fieldFunc, keyPrefix, storage)(BoundedSeq200(ids))
    } else if (ids.length <= 500) {
      getCompiledQuerySeqFunc500(originalQuery, fieldFunc, keyPrefix, storage)(BoundedSeq500(ids))
    } else if (ids.length <= 1000) {
      getCompiledQuerySeqFunc1000(originalQuery, fieldFunc, keyPrefix, storage)(BoundedSeq1000(ids))
    } else {
      throw new Exception("Queries cannot handle more than 1000 items")
    }
  }

  private def getCompiledQuerySeqFunc100(
    originalQuery: QueryWithFilter,
    fieldFunc: T => Rep[DbUUID],
    keyPrefix: String,
    storage: SlickCompiledFunctionSingleton
  ): CompiledFunction[_, _, BoundedSeq100[DbUUID], _, Seq[V]] = {
    storage.getOrInitCompiledQuery(s"${keyPrefix}_100"){
      def funct(
        values: Rep[BoundedSeq100[DbUUID]]
      ): Query[T, V, Seq] = originalQuery.filter(q => fieldFunc(q).inSeqDbUUID100(values))
      Compiled(funct _)
    }
  }

  private def getCompiledQuerySeqFunc200(
    originalQuery: QueryWithFilter,
    fieldFunc: T => Rep[DbUUID],
    keyPrefix: String,
    storage: SlickCompiledFunctionSingleton
  ): CompiledFunction[_, _, BoundedSeq200[DbUUID], _, Seq[V]] = {
    storage.getOrInitCompiledQuery(s"${keyPrefix}_200"){
      def funct(
        values: Rep[BoundedSeq200[DbUUID]]
      ): Query[T, V, Seq] = originalQuery.filter(q => fieldFunc(q).inSeqDbUUID200(values))
      Compiled(funct _)
    }
  }

  private def getCompiledQuerySeqFunc500(
    originalQuery: QueryWithFilter,
    fieldFunc: T => Rep[DbUUID],
    keyPrefix: String,
    storage: SlickCompiledFunctionSingleton
  ): CompiledFunction[_, _, BoundedSeq500[DbUUID], _, Seq[V]] = {
    storage.getOrInitCompiledQuery(s"${keyPrefix}_500"){
      def funct(
        values: Rep[BoundedSeq500[DbUUID]]
      ): Query[T, V, Seq] = originalQuery.filter(q => fieldFunc(q).inSeqDbUUID500(values))
      Compiled(funct _)
    }
  }

  private def getCompiledQuerySeqFunc1000(
    originalQuery: QueryWithFilter,
    fieldFunc: T => Rep[DbUUID],
    keyPrefix: String,
    storage: SlickCompiledFunctionSingleton
  ): CompiledFunction[_, _, BoundedSeq1000[DbUUID], _, Seq[V]] = {
    storage.getOrInitCompiledQuery(s"${keyPrefix}_1000"){
      def funct(
        values: Rep[BoundedSeq1000[DbUUID]]
      ): Query[T, V, Seq] = originalQuery.filter(q => fieldFunc(q).inSeqDbUUID1000(values))
      Compiled(funct _)
    }
  }

  override def getCompiledQuerySeq[A](
    originalQuery: QueryWithFilter,
    fieldFunc: T => Rep[DbUUID],
    keyPrefix: String,
    storage: SlickCompiledFunctionSingleton,
    ids: Seq[DbUUID],
    field1: (T, Rep[A]) => Rep[Boolean],
    field1Value: A
  )(implicit aTpe: P#DriverJdbcType[A]): AppliedCompiledFunction[_, _, Seq[V]] = {
    if (ids.length <= 100) {
      val toCall = storage.getOrInitCompiledQuery(s"${keyPrefix}_100"){
        def funct(
          idValues: Rep[BoundedSeq100[DbUUID]],
          field1Val: Rep[A]
        ): Query[T, V, Seq] = {
          originalQuery.filter(q => fieldFunc(q).inSeqDbUUID100(idValues))
            .filter(q => field1(q, field1Val))
        }
        Compiled(funct _)
      }
      toCall((BoundedSeq100(ids), field1Value))
    } else if (ids.length <= 200){
      val toCall = storage.getOrInitCompiledQuery(s"${keyPrefix}_200") {
        def funct(
          idValues: Rep[BoundedSeq200[DbUUID]],
          field1Val: Rep[A]
        ): Query[T, V, Seq] = {
          originalQuery.filter(q => fieldFunc(q).inSeqDbUUID200(idValues))
            .filter(q => field1(q, field1Val))
        }
        Compiled(funct _)
      }
      toCall((BoundedSeq200(ids), field1Value))
    } else if (ids.length <= 500){
      val toCall = storage.getOrInitCompiledQuery(s"${keyPrefix}_500") {
        def funct(
          idValues: Rep[BoundedSeq500[DbUUID]],
          field1Val: Rep[A]
        ): Query[T, V, Seq] = {
          originalQuery.filter(q => fieldFunc(q).inSeqDbUUID500(idValues))
            .filter(q => field1(q, field1Val))
        }
        Compiled(funct _)
      }
      toCall((BoundedSeq500(ids), field1Value))
    } else if (ids.length <= 1000){
      val toCall = storage.getOrInitCompiledQuery(s"${keyPrefix}_1000") {
        def funct(
          idValues: Rep[BoundedSeq1000[DbUUID]],
          field1Val: Rep[A]
        ): Query[T, V, Seq] = {
          originalQuery.filter(q => fieldFunc(q).inSeqDbUUID1000(idValues))
            .filter(q => field1(q, field1Val))
        }
        Compiled(funct _)
      }
      toCall((BoundedSeq1000(ids), field1Value))
    } else {
      throw new Exception("Queries cannot handle more than 1000 items")
    }
  }

  override def getCompiledQuerySeq[A, B](
    originalQuery: QueryWithFilter,
    fieldFunc: T => Rep[DbUUID],
    keyPrefix: String,
    storage: SlickCompiledFunctionSingleton,
    ids: Seq[DbUUID],
    field1: (T, Rep[A]) => Rep[Boolean],
    field1Value: A,
    field2: (T, Rep[B]) => Rep[Boolean],
    field2Value: B
  )(
    implicit aTpe: P#DriverJdbcType[A],
    bTpe: P#DriverJdbcType[B]
  ): AppliedCompiledFunction[_, _, Seq[V]] = {
    if (ids.length <= 100) {
      val toCall = storage.getOrInitCompiledQuery(s"${keyPrefix}_100") {
        def funct(
          idValues: Rep[BoundedSeq100[DbUUID]],
          field1Val: Rep[A],
          field2Val: Rep[B]
        ): Query[T, V, Seq] = {
          originalQuery.filter(q => fieldFunc(q).inSeqDbUUID100(idValues))
            .filter(q => field1(q, field1Val))
            .filter(q => field2(q, field2Val))
        }
        Compiled(funct _)
      }
      toCall((BoundedSeq100(ids), field1Value, field2Value))
    } else if (ids.length <= 200) {
      val toCall = storage.getOrInitCompiledQuery(s"${keyPrefix}_200") {
        def funct(
          idValues: Rep[BoundedSeq200[DbUUID]],
          field1Val: Rep[A],
          field2Val: Rep[B]
        ): Query[T, V, Seq] = {
          originalQuery.filter(q => fieldFunc(q).inSeqDbUUID200(idValues))
            .filter(q => field1(q, field1Val))
            .filter(q => field2(q, field2Val))
        }

        Compiled(funct _)
      }
      toCall((BoundedSeq200(ids), field1Value, field2Value))
    } else if (ids.length <= 500) {
      val toCall = storage.getOrInitCompiledQuery(s"${keyPrefix}_500") {
        def funct(
          idValues: Rep[BoundedSeq500[DbUUID]],
          field1Val: Rep[A],
          field2Val: Rep[B]
        ): Query[T, V, Seq] = {
          originalQuery.filter(q => fieldFunc(q).inSeqDbUUID500(idValues))
            .filter(q => field1(q, field1Val))
            .filter(q => field2(q, field2Val))
        }
        Compiled(funct _)
      }
      toCall((BoundedSeq500(ids), field1Value, field2Value))
    } else if (ids.length <= 1000) {
      val toCall = storage.getOrInitCompiledQuery(s"${keyPrefix}_1000") {
        def funct(
          idValues: Rep[BoundedSeq1000[DbUUID]],
          field1Val: Rep[A],
          field2Val: Rep[B]
        ): Query[T, V, Seq] = {
          originalQuery.filter(q => fieldFunc(q).inSeqDbUUID1000(idValues))
            .filter(q => field1(q, field1Val))
            .filter(q => field2(q, field2Val))
        }
        Compiled(funct _)
      }
      toCall((BoundedSeq1000(ids), field1Value, field2Value))
    } else {
      throw new Exception("Queries cannot handle more than 1000 items")
    }
  }
}
// scalastyle:on parameter.number
