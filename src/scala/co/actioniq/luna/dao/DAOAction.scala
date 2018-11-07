package co.actioniq.luna.dao

import slick.jdbc.{JdbcProfile, JdbcType}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

/**
  * Take the queries generated and actually perform the action.  Most functions return a nonblocking DBIOAction
  * @tparam T slick table, extends aiqtable
  * @tparam V case class to store result set rows
  * @tparam I id type (option long and uuid)
  */
trait DAOAction[T <: DAOTable.Table[V, I, P], V <: IdModel[I], I <: IdType, P <: JdbcProfile]
  extends DAOQuery[T, V, I, P] with DAOHook[V] with DAOActionValidate[V] {
  protected val profile: JdbcProfile // scalastyle:ignore
  import profile.api._ // scalastyle:ignore

  /**
    * Perform read with join
    * @param other other DAO to join to
    * @param on lambda filter function to specify "on" clause for join
    * @param extraQueryOps extra where clause
    * @tparam A type of other slick table
    * @tparam B type of other slick model
    * @tparam C type of other idtype
    * @return action of join query
    */
  protected def readJoinAction[A <: DAOTable.Table[B, C, P], B <: IdModel[C], C <: IdType]
  (
    other: DAOAction[A, B, C, P],
    on: (T, A) => Rep[Option[Boolean]],
    extraQueryOps: QueryJoin[A, B] => QueryJoin[A, B] = (query: QueryJoin[A, B]) => query
  ):
  DBIOAction[Seq[(V, B)], NoStream, Effect.Read]
  = {
    joinQuery[A, B, C](other, (mine, theirs) => on(mine, theirs), extraQueryOps).result
  }

  protected def readJoinActionTwo[A <: DAOTable.Table[B, C, P], B <: IdModel[C], C <: IdType,
  AA <: DAOTable.Table[BB, CC, P], BB <: IdModel[CC], CC <: IdType]
  (
    otherFirst: DAOAction[A, B, C, P],
    onFirst: (T, A) => Rep[Option[Boolean]],
    otherSecond: DAOAction[AA, BB, CC, P],
    onSecond: (T, A, AA) => Rep[Option[Boolean]],
    extraQueryOps: QueryJoinTwo[A, B, AA, BB] => QueryJoinTwo[A, B, AA, BB]
      = (query: QueryJoinTwo[A, B, AA, BB]) => query
  )(implicit ec: ExecutionContext):
  DBIOAction[Seq[(V, B, BB)], NoStream, Effect.Read]
  = {
    joinQueryTwo[A, B, C, AA, BB, CC](otherFirst, onFirst, otherSecond, onSecond, extraQueryOps).result
      .map { rs =>
        rs.map { row =>
          (row._1._1, row._1._2, row._2)
        }
      }
  }

  /**
    * Perform read with left join
    * @param other other DAO to join to
    * @param on lambda filter function to specify "on" clause for join
    * @param extraQueryOps extra where clause
    * @tparam A type of other slick table
    * @tparam B type of other slick model
    * @tparam C type of other idtype
    * @return action to do left join, "other" piece is option to return
    */
  protected def readLeftJoinAction[A <: DAOTable.Table[B, C, P], B <: IdModel[C], C <: IdType]
  (
    other: DAOAction[A, B, C, P],
    on: (T, A) => Rep[Option[Boolean]],
    extraQueryOps: QueryLeftJoin[A, B] => QueryLeftJoin[A, B] = (query: QueryLeftJoin[A, B]) => query
  ):
  DBIOAction[Seq[(V, Option[B])], NoStream, Effect.Read]
  = {
    leftJoinQuery[A, B, C](other, (mine, theirs) => on(mine, theirs), extraQueryOps).result
  }

  /**
    * Perform a read with child table results.  Think many to many relationships.  This actually runs a query to get
    * results from the "parent", uses that to generate a query for "children", and then merge the data together
    * @param other other DAO that is "child"
    * @param filterChildOn lambda function to filter "child" table on
    * @param merge function to merge both resultsets
    * @param extraQueryOps any extra query params for parent table
    * @param ec
    * @tparam A type of other slick table
    * @tparam B type of other slick model
    * @tparam C type of other idtype
    * @tparam Z return type, typically Seq[(A, Seq[B])]
    * @return
    */
  protected def readWithChildAction[A <: DAOTable.Table[B, C, P], B <: IdModel[C], C <: IdType, Z]
  (
    other: DAOAction[A, B, C, P],
    filterChildOn: (Seq[V], A) => Rep[Option[Boolean]],
    merge: (Seq[V], Seq[B]) => Seq[Z],
    extraQueryOps: (QueryWithFilter)=> QueryWithFilter = (query) => query
  )(implicit ec: ExecutionContext):
  DBIOAction[Seq[Z], NoStream, Effect.Read] = {
    for {
      leftRows <- readAction(extraQueryOps)
      childRows <- other.readAction(q => q.filter(row => filterChildOn(leftRows, row)))
    } yield merge(leftRows, childRows)
  }

  /**
    * Read ids only
    * @param extraQueryOps extra query params
    * @param ec
    * @return dbioaction of sequence of ids
    */
  protected def readIdsAction(
    extraQueryOps: (QueryWithFilter)=> QueryWithFilter = (query) => query
  )(implicit ec: ExecutionContext): DBIOAction[Seq[I], NoStream, Effect.Read] = {
    val query = readQuery
    idMap(extraQueryOps(query)).result
  }

  /**
    * Perform action to read object by id, throw exception if not found
    * @param id id of object
    * @param ec
    * @return dbioaction of model or failure with SlickException
    */
  protected def readByIdRequiredAction(id: I)(implicit ec: ExecutionContext):
  DBIOAction[T#TableElementType, NoStream, Effect.Read]= {
    readByIdQuery(id).result
      .map(_.headOption.getOrElse(throw new SlickException(s"Unknown $nameSingle id: $id")))
  }

  /**
    * Perform action to read object by set of ids, throw exception if any not found
 *
    * @param id set of ids
    * @param ec
    * @throws DAOException if any id is invalid
    * @return dbioaction of sequence of model or failure with SlickException
    */
  protected def readByIdRequiredAction(id: Set[I])(implicit ec: ExecutionContext):
  DBIOAction[Seq[T#TableElementType], NoStream, Effect.Read]= {
    for {
      objects <- readByIdQuery(id).result
      assert =
      if (objects.size == id.size) {
        true
      } else {
          val outputIds = objects.map(_.id).toSet
          throw new SlickException(s"Unkonwn $nameSingle id: ${id.diff(outputIds)}")
      }
    } yield objects
  }

  /**
    * Perform action to read object by id
    * @param id id of object
    * @param ec
    * @return Option of object
    */
  protected def readByIdAction(id: I)(implicit ec: ExecutionContext):
  DBIOAction[Option[T#TableElementType], NoStream, Effect.Read]= {
    readByIdQuery(id).result.map(_.headOption)
  }

  /**
    * Perform action to read objects by set of ids
    * @param id set of ids
    * @param ec
    * @return sequence of objects
    */
  protected def readByIdAction(id: Set[I])(implicit ec: ExecutionContext):
  DBIOAction[Seq[T#TableElementType], NoStream, Effect.Read]= {
    readByIdQuery(id).result
  }

  /**
    * Perform read action
    * @param extraQueryOps extra query params
    * @param ec
    * @return
    */
  protected def readAction(extraQueryOps: (QueryWithFilter)=> QueryWithFilter = (query) => query)
    (implicit ec: ExecutionContext): DBIOAction[Seq[T#TableElementType], NoStream, Effect.Read]= {
    val query = readQuery
    extraQueryOps(query).result
  }

  private def booleanToCreateValidationFunction(toValidate: Boolean)(implicit ec: ExecutionContext) = {
    if (toValidate) {
      (input: V) => validateCreate(input)
    } else {
      (input: V) => DBIO.successful(FormValidatorMessageSeq())
    }
  }


  /**
    * Perform action to create objects
    * @param inputs sequence of inputs
    * @param toValidate run validation on inputs using default validator
    * @param ec
    * @return sequence of ids or failure with exception
    */
  protected def createAction(inputs: Seq[V], toValidate: Boolean)(implicit ec: ExecutionContext):
  DBIOAction[Seq[I], NoStream, Effect.Read with Effect.Write] = {
    createAction(inputs, booleanToCreateValidationFunction(toValidate))
  }

  /**
    * Perform action to create objects
    * @param inputs sequence of inputs
    * @param validationInput run validation function
    * @param ec
    * @return sequence of ids or failure with exception
    */
  protected def createAction(
    inputs: Seq[V],
    validationInput: (V) => DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read]
  )(implicit ec: ExecutionContext):
  DBIOAction[Seq[I], NoStream, Effect.Read with Effect.Write] = {
    val actions = inputs.map(input => {
      createAction(input, validationInput)
    })
    DBIO.sequence(actions)
  }

  /**
    * Perform action to create a single object
    * @param input object to create
    * @param toValidate run validation on input using default validator
    * @param ec
    * @return ID of object inserted or failure with exception
    */
  protected def createAction(input: V, toValidate: Boolean = true)(implicit ec: ExecutionContext):
  DBIOAction[I, NoStream, Effect.Read with Effect.Write] = {
    createAction(input, booleanToCreateValidationFunction(toValidate))
  }

  /**
    * Perform action to create a single object
    * @param input object to create
    * @param validationInput run validation function
    * @param ec
    * @return ID of object inserted or failure with exception
    */
  protected def createAction(
    input: V,
    validationInput: (V) => DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read]
  )(implicit ec: ExecutionContext):
  DBIOAction[I, NoStream, Effect.Read with Effect.Write] = {
    val create = for {
      _ <- validationsToException(validationInput(input))
      insert <- createQuery(input)
    } yield insert
    create.asTry map {
      case Success(v) => {
        addCreateTransaction(v, input)
        v
      }
      case Failure(th) => th match {
        case ex: FormValidatorExceptions =>
          throw new DAOException(message = ex.errors.head.message, code = ex.errors.head.code,
            klass = Some(classOf[FormValidatorExceptions].getName))
        case ex: Throwable => throw ex
      }
    }
  }

  /**
    * For update action try to get the id from input or throw exception
    * @param input
    * @return
    */
  protected def updateGetId(input: V)(implicit ec: ExecutionContext): DBIOAction[I, NoStream, Effect.Read] = {
    DBIO.successful(input.id)
      .map(_.getOrElse(throw new Exception(s"Update $nameSingle missing ID"))
        .asInstanceOf[I]
      )
  }

  /**
    * For update action try to get the original
    * @param id id of object
    * @return Option of the original
    */
  protected def updateGetOriginal(id: I)
    (implicit ec: ExecutionContext): DBIOAction[Option[V], NoStream, Effect.Read] = {
    readAction(query => idEquals(query, id).take(1))
      .map(_.headOption)
  }

  /**
    * For update action log a transaction on success.  On failure through the correct exception
    * @param result result of the chain of actions in update action
    * @return
    */
  protected def updateHandleResult(result: DBIOAction[(I, Option[V], V), NoStream, Effect.Read with Effect.Write])
    (implicit ec: ExecutionContext): DBIOAction[I, NoStream, Effect.Read with Effect.Write] = {
    result.asTry map {
      case Success(v) => {
        addUpdateTransaction(id = v._1, input = v._3, original = v._2.head)
        v._1
      }
      case Failure(th) => th match {
        case ex: FormValidatorExceptions =>
          throw new DAOException(message = ex.errors.head.message, code = ex.errors.head.code,
            klass = Some(classOf[FormValidatorExceptions].getName))
        case ex: Throwable => throw ex
      }
    }
  }

  private def booleanToUpdateValidationFunction(toValidate: Boolean)(implicit ec: ExecutionContext) = {
    if (toValidate) {
      (newAndOriginal: NewAndOriginal[V]) => validateUpdate(newAndOriginal)
    } else {
      (newAndOriginal: NewAndOriginal[V]) => DBIO.successful(FormValidatorMessageSeq())
    }
  }

  /**
    * Perform an update action
    * @param id id of object
    * @param toValidate run validation on input using default validator
    * @param updateFunction function taking in original object returning an end result object to persist
    * @param ec
    * @return id of object or failure with exception
    */
  protected def updateActionFunctional(
    id: I,
    toValidate: Boolean,
    updateFunction: V => V
  )(implicit ec: ExecutionContext): DBIOAction[I, NoStream, Effect.Read with Effect.Write] = {
    val validationFunction = booleanToUpdateValidationFunction(toValidate)
    updateActionFunctional(id, validationFunction, updateFunction)
  }

  /**
    * Perform an update action
    * @param id id of object
    * @param validationInputOriginal run validation on input using validation function
    * @param updateFunction function taking in original object returning an end result object to persist
    * @param ec
    * @return id of object or failure with exception
    */
  protected def updateActionFunctional(
    id: I,
    validationInputOriginal: (NewAndOriginal[V]) => DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read],
    updateFunction: V => V
  )(implicit ec: ExecutionContext): DBIOAction[I, NoStream, Effect.Read with Effect.Write] = {
    for {
      // why am i go doing another get to get the original?  it should already be passed in (possibly)
      originalOpt <- updateGetOriginal(id)
      original = originalOpt match {
        case Some(originalObject) =>
          originalObject
        case _ => throw new Exception(s"Object $nameSingle not found with id $id")
      }
      result <- updateActionFunctional(original, validationInputOriginal, updateFunction)
    } yield result
  }

  /**
    * Perform an update action
    * @param original original object
    * @param toValidate run validation on input using default validator
    * @param updateFunction function taking in original object returning an end result object to persist
    * @param ec
    * @return id of object or failure with exception
    */
  protected def updateActionFunctional(
    original: V,
    toValidate: Boolean,
    updateFunction: V => V
  )(implicit ec: ExecutionContext): DBIOAction[I, NoStream, Effect.Read with Effect.Write] = {
    val validationFunction = booleanToUpdateValidationFunction(toValidate)
    updateActionFunctional(original, validationFunction, updateFunction)
  }

  /**
    * Perform an update action
    * @param original original object
    * @param validationInputOriginal run validation on input using validation function
    * @param updateFunction function taking in original object returning an end result object to persist
    * @param ec
    * @return id of object or failure with exception
    */
  protected def updateActionFunctional(
    original: V,
    validationInputOriginal: (NewAndOriginal[V]) => DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read],
    updateFunction: V => V
  )(implicit ec: ExecutionContext): DBIOAction[I, NoStream, Effect.Read with Effect.Write] = {
    val input = updateFunction(original)
    val newAndOriginal = NewAndOriginal(originalObject = original, newObject = input)
    val action = for {
      inputId <- updateGetId(input)
      validate <- validationsToException(validationInputOriginal(newAndOriginal))
      // At some point we probably don't want to update all columns (updating keys is bad)
      update <- updateQuery(inputId, input)
    } yield (update, Some(original), input)
    updateHandleResult(action)
  }

  /**
    * Perform an update action
    * @param input object to update, id will be extracted
    * @param toValidate run validation on input using default validator
    * @param ec
    * @return id of object or failure with exception
    */
  protected def updateAction(
    input: V,
    toValidate: Boolean,
    inOriginal: Option[V]
  )(implicit ec: ExecutionContext): DBIOAction[I, NoStream, Effect.Read with Effect.Write] = {
    val validationFunction = booleanToUpdateValidationFunction(toValidate)
    updateAction(input, validationFunction, inOriginal)
  }

  /**
    * Perform an update action
    * @param input object to update, id will be extracted
    * @param validationInputOriginal run validation on input using validation function
    * @param ec
    * @return id of object or failure with exception
    */
  protected def updateAction(
    input: V,
    validationInputOriginal: (NewAndOriginal[V]) => DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read],
    inOriginal: Option[V]
  )(implicit ec: ExecutionContext): DBIOAction[I, NoStream, Effect.Read with Effect.Write] = {
    for {
      inputId <- updateGetId(input)
      result <- updateActionFunctional(inputId, validationInputOriginal, (inOriginal: V) => input)
    } yield result
  }

  /**
    *
    * Perform an update action on a seq of values
    * @param inputs objects to update, id will be extracted
    * @param toValidate run validation on input
    * @param ec
    * @return id of object or failure with exceptions
    */
  protected def updateAction(
    inputs: Seq[V],
    toValidate: Boolean,
    originals: Seq[V]
  )(implicit ec: ExecutionContext): DBIOAction[Seq[I], NoStream, Effect.Read with Effect.Write] = {
    val validationFunction = booleanToUpdateValidationFunction(toValidate)
    updateAction(inputs, validationFunction, originals)
  }

  /**
    *
    * Perform an update action on a seq of values
    * @param inputs objects to update, id will be extracted
    * @param validationInputOriginal run validation on input using validation function
    * @param ec
    * @return id of object or failure with exceptions
    */
  protected def updateAction(
    inputs: Seq[V],
    validationInputOriginal: (NewAndOriginal[V]) => DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read],
    originals: Seq[V]
  )(implicit ec: ExecutionContext): DBIOAction[Seq[I], NoStream, Effect.Read with Effect.Write] = {
    val actions = inputs.map(input => {
      updateAction(input, validationInputOriginal, originals.find(_.id == input.id))
    })
    DBIO.sequence(actions)
  }

  /**
    * Update a single field without touching the rest of the row.  UpdateActionFunctional actually replaces the whole
    * row with input mixed with original
    * @param id id of the object
    * @param fieldFunction function to go from row to field
    * @param setTo value to set field to
    * @param validationFieldsOriginal function to validate new field value and original object
    * @param original original object
    * @param ec execution context
    * @param aType evidence of field type
    * @tparam A field type
    * @return id
    */
  protected def updateFieldAction[A](
    id: I,
    fieldFunction:  (T => Rep[A]),
    setTo: A,
    validationFieldsOriginal: (A, V) => DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read],
    original: Option[V]
  )(implicit ec: ExecutionContext, aType: JdbcType[A]): DBIOAction[I, NoStream, Effect.Read with Effect.Write] = {
    for {
      originalObject <- original match {
        case None => readByIdRequiredAction(id)
        case Some(row) => DBIO.successful(row)
      }
      _ <- validationsToException(
        validationFieldsOriginal(setTo, originalObject)
      )
      _ <- updateFieldQuery(id, fieldFunction, setTo)
    } yield id
  }

  /**
    * Update TWO fields without touching the rest of the row.  UpdateActionFunctional actually replaces the whole
    * row with input mixed with original
    * @param id id of the object
    * @param fieldFunction function to go from row to fields
    * @param setTo tuple of values to set fields to
    * @param validationFieldsOriginal function to validate new field values and original object
    * @param original original object
    * @param ec execution context
    * @param a1Type evidence of field 1 type
    * @param a2Type evidence of field 2 type
    * @tparam A1 field 1 type
    * @tparam A2 field 2 type
    * @return
    */
  protected def updateFieldAction[A1, A2](
    id: I,
    fieldFunction:  (T => (Rep[A1], Rep[A2])),
    setTo: (A1, A2),
    validationFieldsOriginal: ((A1, A2), V) => DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read],
    original: Option[V]
  )(
    implicit ec: ExecutionContext,
    a1Type: JdbcType[A1],
    a2Type: JdbcType[A2]
  ): DBIOAction[I, NoStream, Effect.Read with Effect.Write] = {
    for {
      originalObject <- original match {
        case None => readByIdRequiredAction(id)
        case Some(row) => DBIO.successful(row)
      }
      _ <- validationsToException(
        validationFieldsOriginal(setTo, originalObject)
      )
      _ <- updateFieldQuery(id, fieldFunction, setTo)
    } yield id
  }

  /**
    * Update THREE fields without touching the rest of the row.  UpdateActionFunctional actually replaces the whole
    * row with input mixed with original
    * @param id id of the object
    * @param fieldFunction function to go from row to fields
    * @param setTo tuple of values to set fields to
    * @param validationFieldsOriginal function to validate new field values and original object
    * @param original original object
    * @param ec execution context
    * @param a1Type evidence of field 1 type
    * @param a2Type evidence of field 2 type
    * @param a3Type evidence of field 3 type
    * @tparam A1 field 1 type
    * @tparam A2 field 2 type
    * @tparam A3 field 3 type
    * @return
    */
  protected def updateFieldAction[A1, A2, A3](
    id: I,
    fieldFunction:  (T => (Rep[A1], Rep[A2], Rep[A3])),
    setTo: (A1, A2, A3),
    validationFieldsOriginal: ((A1, A2, A3), V) => DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read],
    original: Option[V]
  )(
    implicit ec: ExecutionContext,
    a1Type: JdbcType[A1],
    a2Type: JdbcType[A2],
    a3Type: JdbcType[A3]
  ): DBIOAction[I, NoStream, Effect.Read with Effect.Write] = {
    for {
      originalObject <- original match {
        case None => readByIdRequiredAction(id)
        case Some(row) => DBIO.successful(row)
      }
      _ <- validationsToException(
        validationFieldsOriginal(setTo, originalObject)
      )
      _ <- updateFieldQuery(id, fieldFunction, setTo)
    } yield id
  }

  /**
    * Perform a delete action
    * @param inputId id of object to delete
    * @param ec
    * @return number of rows deleted or failure with exception
    */
  protected def deleteAction(inputId: I)(implicit ec: ExecutionContext):
  DBIOAction[Int, NoStream, Effect.Read with Effect.Write] = {
    for {
      original <- readAction(query => idEquals(query, inputId).take(1))
      validate = original match {
        case Nil => throw new Exception(s"Object $nameSingle not found with id $inputId")
        case _ => true
      }
      delete <- deleteQuery(inputId).map(result => {
        addDeleteTransaction(inputId, original.head)
        result
      })
    } yield delete
  }

  /**
    *  Perform a delete action on a seq of values
    *  @param inputIds ids of objects to delete
    *  @param ec
    *  @return number of rows deleted for each input
    */
  protected def deleteAction(inputIds: Seq[I])(implicit ec: ExecutionContext):
  DBIOAction[Seq[Int], NoStream, Effect.Read with Effect.Write] = {
    val actions = inputIds.map(inputId => {
      deleteAction(inputId)
    })
    DBIO.sequence(actions)
  }


  /**
    * Add to transaction log a create operation
    * @param id id of object
    * @param input input
    */
  protected def addCreateTransaction(id: I, input: V): Unit

  /**
    * Add to transaction log an update operation
    * @param id id of object
    * @param input new object
    * @param original original object
    */
  protected def addUpdateTransaction(id: I, input: V, original: V): Unit

  /**
    * Add to transaction log a delete operation
    * @param id id of object
    * @param original original object
    */
  protected def addDeleteTransaction(id: I, original: V): Unit
}

/**
  * Trait with functions to implement to perform validations.  Functions need to return DBIOActions so that they can
  * be included in transactions.  You would probably do DBIO.successful(....)
  * @tparam V model / slick case class
  */
trait DAOActionValidate[V] {
  protected val profile: JdbcProfile
  import profile.api._ // scalastyle:ignore
  /**
    * Validate create
    * @param input input to validate
    * @param ec
    * @return DBIOAction of FormValidatorMessageSeq
    */
  def validateCreate(input: V)(implicit ec: ExecutionContext):
  DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read]

  /**
    * Validate an update
    * @param input input to validate
    * @param original original object
    * @param ec
    * @return DBIOAction of FormValidatorMessageSeq
    */
  def validateUpdate(input: V, original: V)(implicit ec: ExecutionContext):
  DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read]

  /**
    * Validate an input using NewAndOriginal wrapper
    * @param newAndOriginal wrapper around new and original value
    * @param ec
    * @return DBIOAction of FormValidatorMessageSeq
    */
  def validateUpdate(newAndOriginal: NewAndOriginal[V])(implicit ec: ExecutionContext):
  DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read] = {
    validateUpdate(newAndOriginal.newObject, newAndOriginal.originalObject)
  }

  /**
    * Throw exception if validations fails
    * @param results DBIOAction of FormValidatorSeq from validation function
    * @param ec execution context
    * @return nothing or failure with FormValidatorExceptions
    */
  def validationsToException(
    results: DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read]
  ) (implicit ec: ExecutionContext): DBIOAction[Unit, NoStream, Effect.Read] = {
    results map {
      case FormValidatorMessageSeq(Seq()) => Unit
      case items: FormValidatorMessageSeq => throw new FormValidatorExceptions(items)
    }
  }
}

/**
  * Simple wrapper to pass new and original objects.  Using a case class makes it more explicit so you know which is
  * new and which is original
  * @param newObject
  * @param originalObject
  * @tparam V
  */
case class NewAndOriginal[V](newObject: V, originalObject: V)
