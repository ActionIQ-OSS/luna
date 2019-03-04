package co.actioniq.luna.dao


import co.actioniq.luna.{DBWithLogging, OptLongCompare, OptionCompareOption, UUIDCompare}
import slick.jdbc.{H2Profile, JdbcProfile, JdbcType, MySQLProfile, PostgresProfile}

import scala.concurrent.{ExecutionContext, Future}

/**
  * Top of the DAO traits, this actually runs the actions in a transaction.  Most functions return a scala future
  * @tparam T slick table, extends aiqtable
  * @tparam V case class to store result set rows
  * @tparam I id type (option long and uuid)
  */
trait DAO[T <: DAOTable.Table[V, I, P], V <: IdModel[I], I <: IdType, P <: JdbcProfile]
  extends DAOAction[T, V, I, P] {

  protected val db: DBWithLogging
  protected implicit val ec: ExecutionContext

  import profile.api._ // scalastyle:ignore

  implicit def uuidCompare(e: Rep[DbUUID]): UUIDCompare = OptionCompareOption.uuidCompare(e)
  implicit def optLongCompare(e: Rep[DbLongOptId]): OptLongCompare = OptionCompareOption.optLongCompare(e)

  /**
    * Wrapper for running a trx.... SOON WE CAN PUT SOME AUDIT LOGGING IN
    * @param a DBIOAction
    * @tparam R result type
    * @return result
    */
  protected def runTransactionFuture[R](a: DBIOAction[R, NoStream, Effect.All]): Future[R] = {
    db.run(a.transactionally)
  }

  /**
    * Run a join
    * @param other other DAO to join to
    * @param on lambda filter function to specify "on" clause for join
    * @param extraQueryOps extra where clause
    * @tparam A type of other slick table
    * @tparam B type of other slick model
    * @tparam C type of other idtype
    * @return Future of Seq of (mine, theirs)
    */
  def readJoinFuture[A <: DAOTable.Table[B, C, P], B <: IdModel[C], C <: IdType]
  (
    other: DAO[A, B, C, P],
    on: (T, A) => Rep[Option[Boolean]],
    extraQueryOps: QueryJoin[A, B] => QueryJoin[A, B] = (query: QueryJoin[A, B]) => query
  ): Future[Seq[(T#TableElementType, A#TableElementType)]]
  = {
    runTransactionFuture(readJoinAction[A, B, C](other, on, extraQueryOps))
  }

  def readJoinTwoFuture[A <: DAOTable.Table[B, C, P], B <: IdModel[C], C <: IdType,
  AA <: DAOTable.Table[BB, CC, P], BB <: IdModel[CC], CC <: IdType]
  (
    otherFirst: DAO[A, B, C, P],
    onFirst: (T, A) => Rep[Option[Boolean]],
    otherSecond: DAO[AA, BB, CC, P],
    onSecond: (T, A, AA) => Rep[Option[Boolean]],
    extraQueryOps: QueryJoinTwo[A, B, AA, BB] => QueryJoinTwo[A, B, AA, BB]
      = (query: QueryJoinTwo[A, B, AA, BB]) => query
  ): Future[Seq[(T#TableElementType, A#TableElementType, AA#TableElementType)]]
  = {
    runTransactionFuture(readJoinActionTwo[A, B, C, AA, BB, CC](
      otherFirst,
      onFirst,
      otherSecond,
      onSecond,
      extraQueryOps
    ))
  }

  /**
    * Run a left join
    * @param other other DAO to join to
    * @param on lambda filter function to specify "on" clause for join
    * @tparam A type of other slick table
    * @tparam B type of other slick model
    * @tparam C type of other idtype
    * @return future of Seq of (min, option(theirs))
    */
  def readLeftJoinFuture[A <: DAOTable.Table[B, C, P], B <: IdModel[C], C <: IdType]
  (other: DAO[A, B, C, P], on: (T, A) => Rep[Option[Boolean]]):
  Future[Seq[(T#TableElementType, Option[A#TableElementType])]]
  = {
    runTransactionFuture(readLeftJoinAction[A, B, C](other, on))
  }

  /**
    * This function is used for "joining" the results of another DAO with this DAO.  It is used for many to many
    * relationships (usually parent to child).  It is meant to allow you to have for each row of this DAO, N results
    * from another DAO.  Example (this DAO is FlightPlanTable + FlightPlanModel):
    *
    *     readWithChild[FlightTable, FlightModel, Array[Byte], (FlightPlanModel, Seq[FlightModel])](
      other = flightDao,
      filterChildOn = (flightPlans, flightTable) => {
        flightTable.flightPlanId inSet flightPlans.map(_.id.get)
      },
      merge = (flightPlans, flights) => {
        val flightsMap = flights.groupBy(_.flightPlanId)
        flightPlans.map(plan => (plan, flightsMap.getOrElse(plan.id.get, Seq())))
      }
    )
    * @param other the other DAO (child)
    * @param filterChildOn lambda function to add a SQL filter to the child DAO.  Typical usecase is if you have
    *                      parent to child relationship, the child dao will filter on child.parent_id in (parent.ids).
    *                      The parameters are: sequence of parent objects, child table
    * @param merge lambda function to merge this DAO's results with the child DAO's results.  Typically you would
    *              want something like Seq[(ParentModel, Seq[ChildModel])]
    * @param extraQueryOps optional extra filters for this DAO
    * @tparam A other Table
    * @tparam B other Model
    * @tparam C other ID type (String, Long, Array[Byte])
    * @tparam Z return type, for example Seq[(V, Seq[B])]
    * @return
    */
  def readWithChildFuture[A <: DAOTable.Table[B, C, P], B <: IdModel[C], C <: IdType, Z]
  (
    other: DAO[A, B, C, P],
    filterChildOn: (Seq[V], A) => Rep[Option[Boolean]],
    merge: (Seq[V], Seq[B]) => Seq[Z],
    extraQueryOps: (QueryWithFilter)=> QueryWithFilter = (query) => query
  ):
  Future[Seq[Z]] = {
    runTransactionFuture(readWithChildAction(other, filterChildOn, merge, extraQueryOps))
  }

  /**
    * Run a read
    * @param extraQueryOps extra filters / limits
    * @return future of seq of model
    */
  def readFuture(extraQueryOps: (QueryWithFilter)=> QueryWithFilter = (query) => query): Future[Seq[V]] = {
    runTransactionFuture(readAction(extraQueryOps))
  }

  /**
    * Run a read but only retrieve ids
    * @param extraQueryOps extra filters / limits
    * @return future of seq of ids
    */
  def readIdsFuture(extraQueryOps: (QueryWithFilter)=> QueryWithFilter = (query) => query): Future[Seq[I]] = {
    runTransactionFuture(readIdsAction(extraQueryOps))
  }

  /**
    * Run a read by id
    * @param id object id
    * @return Future of option of model
    */
  def readByIdFuture(id: I): Future[Option[V]] = {
    runTransactionFuture(readByIdAction(id))
  }

  /**
    * Run a read by set of ids
    * @param id set of ids
    * @return future of seq of model
    */
  def readByIdFuture(id: Set[I]): Future[Seq[V]] = {
    runTransactionFuture(readByIdAction(id))
  }

  /**
    * Run a read by id requiring id to exist
    * @param id object id
    * @return future of model
    */
  def readByIdRequiredFuture(id: I): Future[V] = {
    runTransactionFuture(readByIdRequiredAction(id))
  }

  /**
    * Run a read by set of ids requiring ids exist
    * @param id set of object ids
    * @return future of seq of models
    */
  def readByIdRequiredFuture(id: Set[I]): Future[Seq[V]] = {
    runTransactionFuture(readByIdRequiredAction(id))
  }

  /**
    * Run a create
    * @param input model to create
    * @return future of id
    */
  def createFuture(input: V): Future[I] = {
    runTransactionFuture(createAction(processPreCreate(input), true))
  }

  /**
    * Run a create and return object
    * @param input model to create
    * @return future of object persisted
    */
  def createAndReadFuture(input: V): Future[V] = {
    val actions = for {
      create <- createAction(processPreCreate(input))
      row <- readByIdRequiredAction(create)
    } yield processPostCreate(row)
    runTransactionFuture(actions)
  }

  /**
    * Run a create on multiple objects in trx
    * @param input sequence of models
    * @return future of set of ids
    */
  def createFuture(input: Seq[V]): Future[Seq[I]] = {
    runTransactionFuture(createAction(input.map(processPreCreate), true))
  }

  /**
    * Run a create on multiple objects in trx and return objects
    * @param input seq of objects to save
    * @return future of seq of objects saved
    */
  def createAndReadFuture(input: Seq[V]): Future[Seq[V]] = {
    val actions = for {
      create <- createAction(input.map(processPreCreate), true)
      rows <- readAction(query => idInSet(query, create))
    } yield rows.map(processPostCreate)
    runTransactionFuture(actions)
  }

  /**
    * Run an update
    * @param input object to update
    * @return future of object id
    */
  def updateFuture(input: V): Future[I] = {
    val actions = for {
      original <- readByIdAction(input.id)
      processedInput = processPreUpdate(input, original)
      update <- updateAction(processedInput, true, original)
    } yield update
    runTransactionFuture(actions)
  }

  /**
    * Run an update and return persisted object
    * @param input object to update
    * @return future of object persisted
    */
  def updateAndReadFuture(input: V): Future[V] = {
    val actions = for {
      original <- updateGetOriginal(input.id)
      processedInput = processPreUpdate(input, original)
      update <- updateAction(processedInput, true, original)
      row <- readByIdAction(input.id).map(_.get)
    } yield processPostUpdate(row)
    runTransactionFuture(actions)
  }

  /**
    * Run an update and return persisted object
    * @param id id of object to update
    * @param updateFn a function that updates the object
    * @return future of object persisted
    */
  def updateAndReadFuture(id: I, updateFn: V => V): Future[V] = {
    val actions = for {
      updateId <- updateActionFunctional(id, toValidate = true, updateFn)
      row <- readByIdRequiredAction(updateId)
    } yield row
    runTransactionFuture(actions)
  }

  /**
    * Run an update on a sequence and return persisted object
    * @param queryOps: (QueryWithFilter) => QueryWithFilter = (query) => query
    * @param updateFn a function that updates the object
    * @return future of object persisted
    */
  def queryAndUpdateFuture(
    queryOps: (QueryWithFilter) => QueryWithFilter,
    updateFn: Seq[V] => Seq[V]
  ): Future[Seq[I]] = {
    val actions = for {
      rows <- readAction(queryOps)
      updatedModels = updateFn(rows)
      update <- updateAction(updatedModels, toValidate = true, rows)
    } yield update
    runTransactionFuture(actions)
  }

  /**
    * Run an update on multiple objects in a trx
    * @param input seq of models to persist
    * @return future seq of ids
    */
  def updateFuture(input: Seq[V]): Future[Seq[I]] = {
    val ids = input.map(_.id)
    val actions = for {
      originals <- readAction(query => idInSet(query, ids))
      processedInputs = input.map { item =>
        val original = originals.find(_.id == item.id)
        processPreUpdate(item, original)
      }
      updates <- updateAction(processedInputs, true, originals)
    } yield updates
    runTransactionFuture(actions)
  }

  /**
    * Run an update on multiple object in a trx and return the persisted objects
    * @param input seq of models to persist
    * @return future of seq of objects
    */
  def updateAndReadFuture(input: Seq[V]): Future[Seq[V]] = {
    val ids = input.map(_.id)
    val actions = for {
      originals <- readAction(query => idInSet(query, ids))
      processedInputs = input.map { item =>
        val original = originals.find(_.id == item.id)
        processPreUpdate(item, original)
      }
      updates <- updateAction(processedInputs, true, originals)
      rows <- readAction(query => idInSet(query, ids))
    } yield rows.map(processPostUpdate)
    runTransactionFuture(actions)
  }

  /**
    * Update a single field without touching the rest of the row.  UpdateActionFunctional actually replaces the whole
    * row with input mixed with original
    * @param id id of the object
    * @param fieldFunction function to go from row to field
    * @param setTo value to set field to
    * @param validationFieldsOriginal function to validate new field value and original object
    * @param original original object
    * @tparam A field type
    * @return future of id
    */
  def updateFieldFuture[A](
    id: I,
    fieldFunction:  (T => Rep[A]),
    setTo: A,
    validationFieldsOriginal: (A, V) => DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read],
    original: Option[V]
  )(implicit aType: JdbcType[A]): Future[I] = {
    runTransactionFuture(updateFieldAction(id, fieldFunction, setTo, validationFieldsOriginal, original))
  }

  /**
    * Update two fields without touching the rest of the row.  UpdateActionFunctional actually replaces the whole
    * row with input mixed with original
    * @param id id of the object
    * @param fieldFunction function to go from row to fields
    * @param setTo tuple value to set fields to
    * @param validationFieldsOriginal function to validate new field values and original object
    * @param original original object
    * @tparam A1 field 1 type
    * @tparam A2 field 2 type
    * @return future of id
    */
  def updateFieldFuture[A1, A2](
    id: I,
    fieldFunction:  (T => (Rep[A1], Rep[A2])),
    setTo: (A1, A2),
    validationFieldsOriginal: ((A1, A2), V) => DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read],
    original: Option[V]
  )(implicit a1Type: JdbcType[A1], a2Type: JdbcType[A2]): Future[I] = {
    runTransactionFuture(updateFieldAction(id, fieldFunction, setTo, validationFieldsOriginal, original))
  }

  /**
    * Update three fields without touching the rest of the row.  UpdateActionFunctional actually replaces the whole
    * row with input mixed with original
    * @param id id of the object
    * @param fieldFunction function to go from row to fields
    * @param setTo tuple value to set fields to
    * @param validationFieldsOriginal function to validate new field values and original object
    * @param original original object
    * @tparam A1 field 1 type
    * @tparam A2 field 2 type
    * @tparam A3 field 3 type
    * @return future of id
    */
  def updateFieldFuture[A1, A2, A3](
    id: I,
    fieldFunction:  (T => (Rep[A1], Rep[A2], Rep[A3])),
    setTo: (A1, A2, A3),
    validationFieldsOriginal: ((A1, A2, A3), V) => DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read],
    original: Option[V]
  )(implicit a1Type: JdbcType[A1], a2Type: JdbcType[A2], a3Type: JdbcType[A3]): Future[I] = {
    runTransactionFuture(updateFieldAction(id, fieldFunction, setTo, validationFieldsOriginal, original))
  }

  /**
    * Run a delete
    * @param inputId object id
    * @return future of number of rows updated
    */
  def deleteFuture(inputId: I): Future[Int] = {
    runTransactionFuture(deleteAction(inputId))
  }

  /**
    * Run a delete
    * @param inputIds seq of object ids
    * @return future of number of rows updated
    */
  def deleteFuture(inputIds: Seq[I]): Future[Seq[Int]] = {
    runTransactionFuture(deleteAction(inputIds))
  }
}

trait MySQLDAO[T <: MySQLDAOTable[V, I], V <: IdModel[I], I <: IdType] extends DAO[T, V, I, DAOMySQLProfile]
  with JdbcTypeImplicits.mySQLJdbcTypeImplicits.DbImplicits

trait PostgresDAO[T <: PostgresDAOTable[V, I], V <: IdModel[I], I <: IdType] extends DAO[T, V, I, PostgresProfile]
  with JdbcTypeImplicits.postgresJdbcTypeImplicits.DbImplicits

trait H2DAO[T <: H2DAOTable[V, I], V <: IdModel[I], I <: IdType] extends DAO[T, V, I, DAOH2Profile]
  with JdbcTypeImplicits.h2JdbcTypeImplicits.DbImplicits{
  override protected val profile = DAOH2Profile
}
