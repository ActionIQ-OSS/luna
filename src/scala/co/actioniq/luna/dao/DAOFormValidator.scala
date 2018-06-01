package co.actioniq.luna.dao

import slick.dbio.{DBIOAction, Effect, NoStream}
import slick.jdbc.JdbcBackend.Database
import slick.jdbc.JdbcProfile

import scala.concurrent.ExecutionContext

/**
  * Simple form validator, we should probably spend more time on this and make it better.  Idea is that we can
  * validate create or update, run db queries, and handle multiple validation errors
  * @tparam V case class recordset
  */
trait DAOFormValidator[V] {
  protected val profile: JdbcProfile
  protected val db: Database

  /**
    * Validate a create operation
    * @param input new row to persist
    * @param ec implicit execution context for mapping
    * @return FormValidatorMessageSeq wraps a seq that contains 0 (no errors) to many errors
    */
  def validateCreate(input: V)(implicit ec: ExecutionContext):
  DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read]

  /**
    * Validate an update operation
    * @param input new row to update to
    * @param original original row
    * @param ec implicit execution context for mapping
    * @return FormValidatorMessageSeq wraps a seq that contains 0 (no errors) to many errors
    */
  def validateUpdate(input: V, original: V)(implicit ec: ExecutionContext):
  DBIOAction[FormValidatorMessageSeq, NoStream, Effect.Read]

  /**
    * Validate a create operation and throw an exception on failure
    * @param input new row to persist
    * @param ec implicit execution context for mapping
    * @throws FormValidatorExceptions on any validation failing
    * @return nothing
    */
  @throws[FormValidatorExceptions]
  def validateCreateToException(input: V)(implicit ec: ExecutionContext): DBIOAction[Unit, NoStream, Effect.Read] = {
    validateCreate(input) map {
      case FormValidatorMessageSeq(Seq()) => Unit
      case items: FormValidatorMessageSeq => throw new FormValidatorExceptions(items)
    }
  }

  /**
    * Validate an update operation and throw an exception on failure
    * @param input new row to update to
    * @param original original row
    * @param ec implicit execution context for mapping
    * @throws FormValidatorExceptions on any validation failing
    * @return nothing
    */
  @throws[FormValidatorExceptions]
  def validateUpdateToException(input: V, original: V)(implicit ec: ExecutionContext):
  DBIOAction[Unit, NoStream, Effect.Read] = {
    validateUpdate(input, original) map {
      case FormValidatorMessageSeq(Seq()) => Unit
      case items: FormValidatorMessageSeq => throw new FormValidatorExceptions(items)
    }
  }
}

/**
  * Exception class that wraps multiple validation errors
  * @param errors form validation failures
  */
class FormValidatorExceptions(val errors: FormValidatorMessageSeq)
  extends Exception(errors.errors.map(_.message).mkString(", ")) {
  /**
    * New exception from single message
    * @param error error message
    * @return
    */
  def this(error: String) = this(new FormValidatorMessageSeq(error))

  /**
    * New exception from error and code
    * @param error error message
    * @param code code
    * @return
    */
  def this(error: String, code: String) = this(new FormValidatorMessageSeq(error, code))
}

/**
  * Wrapper class around a seq of form validation messages / failures.  Empty means form validated correctly
  * @param errors seq of form validator messages
  */
case class FormValidatorMessageSeq(var errors: Seq[FormValidatorMessage] = Nil){
  /**
    * New class from single error message
    * @param error error message
    * @return
    */
  def this(error: String) = this(Seq(FormValidatorMessage(error)))

  /**
    * New class from error message and code
    * @param error error message
    * @param code code
    * @return
    */
  def this(error: String, code: String) = this(Seq(FormValidatorMessage(error, Some(code))))

  /**
    * Helper function to assert something and on failure add message to errors
    * @param boolean thing to assert is true
    * @param message error message to add to errors on failure
    * @param code optional code to add
    */
  def assert(boolean: Boolean, message: String, code: Option[String] = None): Unit = {
    if (!boolean){
      addError(FormValidatorMessage(message, code))
    }
  }

  /**
    * Add an error to the list
    * @param error
    */
  def addError(error: FormValidatorMessage): Unit = {
    errors = errors:+ error
  }

  /**
    * Add an error to the list just by message
    * @param message
    */
  def addError(message: String): Unit = {
    errors = errors:+ FormValidatorMessage(message)
  }

  /**
    * Head of errors
    * @return
    */
  def head: FormValidatorMessage = errors.head

  /**
    * Are there errors
    * @return
    */
  def isEmpty: Boolean = errors.isEmpty
}

/**
  * Class wrapping validation error messages
  * @param message error message to return to user
  * @param code optional code for easier parsing
  */
case class FormValidatorMessage(message: String, code: Option[String] = None)
