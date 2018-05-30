package co.actioniq.luna.dao

/*
  This file contains traits for "models" / case classes that store recordsets from slick
 */

/**
  * Any DAO model that has an id
  * @tparam I IdType
  */
trait IdModel[I <: IdType] {
  def id: I
}

trait H2IdModel[I <: IdType] extends IdModel[I] with JdbcTypeImplicits.h2JdbcTypeImplicits.DbImplicits

trait MySQLIdModel[I <: IdType] extends IdModel[I] with JdbcTypeImplicits.mySQLJdbcTypeImplicits.DbImplicits

trait PostgresIdModel[I <: IdType] extends IdModel[I] with JdbcTypeImplicits.postgresJdbcTypeImplicits.DbImplicits









