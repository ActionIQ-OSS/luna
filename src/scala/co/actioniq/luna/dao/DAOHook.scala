package co.actioniq.luna.dao

/**
  * Trait that adds the ability to create functional hooks at different stages in write operations (for altering
  * data or failing)
  * @tparam V case class for recordsets
  */
trait DAOHook[V] {
  def processPreCreate(input: V): V = input
  def processPostCreate(input: V): V = input
  def processPreUpdate(input: V, original: Option[V]): V = input
  def processPostUpdate(input: V): V = input
}


