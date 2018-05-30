package co.actioniq.luna.dao

class DAOException(
  val message: String,
  val klass: Option[String] = None,
  val code: Option[String] = None,
  val file: Option[String] = None,
  val line: Option[Int] = None
) extends Exception(message)
