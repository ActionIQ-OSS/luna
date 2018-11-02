package co.actioniq.luna.dao

import slick.jdbc.JdbcProfile

import scala.concurrent.Future

trait SoftDeleteFilter[T <: DAOTable.Table[V, I, P], V <: IdModel[I], I <: IdType, P <: JdbcProfile, D] extends DefaultFilter[T, V, I, P] {
  protected val profile: JdbcProfile
  import profile.api._
  def softNotDeletedFilter(t: T): Rep[Option[Boolean]]
  def softDeleteFilterField(t: T)(implicit fType: slick.jdbc.JdbcType[D]): Rep[D]
  def softDeleteFilterUpdate(): D
  addDefaultOptFilter(t => softNotDeletedFilter(t))
}

trait SoftDeleteFilterOptional[T <: DAOTable.Table[V, I, P], V <: IdModel[I], I <: IdType, P <: JdbcProfile, D] extends DefaultFilter[T, V, I, P] {
  protected val profile: JdbcProfile
  import profile.api._
  def softNotDeletedFilter(t: T): Rep[Option[Boolean]]
  def softDeleteFilterField(t: T)(implicit fType: slick.jdbc.JdbcType[D]): Rep[Option[D]]
  def softDeleteFilterUpdate(): Option[D]
  addDefaultOptFilter(t => softNotDeletedFilter(t))
}

trait SoftDelete[T <: DAOTable.Table[V, I, P], V <: IdModel[I], I <: IdType, P <: JdbcProfile, D] extends DAO[T, V, I, P] with SoftDeleteFilter[T, V, I, P, D] {
  protected val profile: JdbcProfile
  import profile.api._
  def softDeleteAction(id: I)(implicit fType: slick.jdbc.JdbcType[D]): DBIOAction[Int, NoStream, Effect.Write] = {
    val row = readByIdQuery(id).map(t => softDeleteFilterField(t))
    row.update(softDeleteFilterUpdate())
  }

  def softDeleteFuture(id: I)(implicit fType: slick.jdbc.JdbcType[D]): Future[Int] = {
    runTransactionFuture(softDeleteAction(id))
  }
}

trait SoftDeleteOptional[T <: DAOTable.Table[V, I, P], V <: IdModel[I], I <: IdType, P <: JdbcProfile, D] extends DAO[T, V, I, P] with SoftDeleteFilterOptional[T, V, I, P, D] {
  protected val profile: JdbcProfile
  import profile.api._
  def softDeleteAction(id: I)(implicit fType: slick.jdbc.JdbcType[D]): DBIOAction[Int, NoStream, Effect.Write] = {
    val row = readByIdQuery(id).map(t => softDeleteFilterField(t))
    row.update(softDeleteFilterUpdate())
  }

  def softDeleteFuture(id: I)(implicit fType: slick.jdbc.JdbcType[D]): Future[Int] = {
    runTransactionFuture(softDeleteAction(id))
  }
}
