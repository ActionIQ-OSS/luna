package co.actioniq.luna.dao

import java.util.UUID

/**
  * Defines the type of id that a table will have.  Right now we have some with Option[Long] and some with UUID
  */
trait IdType {
  /**
    * Typical option getOrElse but will work on any IdType
    * @param default default value if the id is not defined
    * @tparam B
    * @return
    */
  @inline def getOrElse[B <: IdType](default: => B): IdType

  /**
    * Specifies if the id value is defined
    * @return
    */
  def isDefined: Boolean
}

/**
  * IdType that represents an id of Option[Long]
  * @param value underlying value (Option[Long])
  */
case class DbLongOptId(value: Option[Long]) extends IdType {
  /**
    *
    * @param default default value if the id is not defined
    * @tparam B
    * @return
    */
  @inline override def getOrElse[B <: IdType](default: => B): IdType = value match {
    case Some(_) => this
    case None => default
  }

  /**
    * Helper to get the Long stored in the option
    * @return
    */
  def get: Long = value.get

  /**
    * Specifies if the id value in the option is defined
    * @return
    */
  override def isDefined: Boolean = value.isDefined

  override def toString: String = value.map(_.toString).orNull
}

object DbLongOptId {
  /**
    * Helper function to create a DbLongOptId from a Long
    * @param value id value
    * @return
    */
  def apply(value: Long): DbLongOptId = {
    DbLongOptId(Option(value))
  }
}

/**
  * IdType that represents a UUID stored as an array[byte] / binary
  * @param binValue array[byte] representing the UUID
  */
case class DbUUID(binValue: Array[Byte]) extends IdType {
  /**
    * Convert array[byte] to typical string representation of UUID
    * @return
    */
  override def toString: String = {
    val hexmap = binValue.map("%02X" format _).mkString
    hexmap.slice(0, 8).mkString.toLowerCase + "-" +
      hexmap.slice(8, 12).mkString.toLowerCase + "-" +
      hexmap.slice(12, 16).mkString.toLowerCase + "-" +
      hexmap.slice(16, 20).mkString.toLowerCase + "-" +
      hexmap.slice(20, 32).mkString.toLowerCase
  }


  override def hashCode(): Int = toString.hashCode

  override def equals(obj: Any): Boolean = {
    this.toString.equals(obj.asInstanceOf[DbUUID].toString)
  }

  /**
    * Since UUID isn't optional, the function always just returns this
    *
    * @param default default value if the id is not defined
    * @tparam B
    * @return
    */
  @inline override def getOrElse[B <: IdType](default: => B): IdType = this

  /**
    * Always true
    * @return
    */
  override def isDefined: Boolean = true
}

object DbUUID{
  /**
    * Helper function to create DbUUID from typical string representation of UUID
    * @param strValue
    * @return
    */
  def apply(strValue: String): DbUUID = {
    val noDash = strValue.replaceAll("-", "")
    val len = noDash.length
    val data = new Array[Byte](len / 2)
    var i = 0
    while (i < len) {
      data(i / 2) = ((Character.digit(noDash.charAt(i), 16) << 4)
        + Character.digit(noDash.charAt(i + 1), 16)).toByte
      i += 2
    }
    new DbUUID(data)
  }

  def randomDbUUID: DbUUID = DbUUID(UUID.randomUUID.toString)

  def apply(intValue: Int): DbUUID = {
    apply(toUUIDString(intValue))
  }

  def toUUIDString(intValue: Int): String = {
    UUID.nameUUIDFromBytes(BigInt(intValue).toByteArray).toString
  }

  def toUUIDString(longValue: Long): String = {
    UUID.nameUUIDFromBytes(BigInt(longValue).toByteArray).toString
  }

  def toUUIDString(customerId: Long, longValue: Long): String = {
    UUID.nameUUIDFromBytes(s"${customerId}_$longValue".getBytes).toString
  }

  def apply(customerId: Long, longValue: Long): DbUUID = {
    apply(toUUIDString(customerId, longValue))
  }

  def apply(longValue: Long): DbUUID = {
    apply(toUUIDString(longValue))
  }

}
