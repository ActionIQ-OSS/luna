package co.actioniq.luna.compiled

import co.actioniq.luna.dao.{DbLongOptId, DbUUID}
import slick.ast.{Library, QueryParameter, TypedType}
import slick.jdbc.JdbcProfile
import slick.lifted.OptionMapperDSL


/**
  * The ugly way to do a compiled query with "in" clause.  This code will create a compiled query with an in clause
  * and 100 ?s since at compile time you do not know how many inputs you will have.
  * Your code will fill in as many ?s as possible and then the rest will be an empty value, which should
  * not affect the resultset.
  */
trait InSeqDbUUIDImplicits {
  protected val profile: JdbcProfile // scalastyle:ignore
  import profile.api._ //scalastyle:ignore

  val optWrapper = new SeqLongOptJdbcType(profile)
  val uuidWrapper = new SeqDbUUIDJdbcType(profile)
  val placeholderWrapper = new PlaceholderJdbcType(profile)

  protected implicit def seqLongOptImplicit = new optWrapper.SeqLongOptJdbcTypeBase
  protected implicit def seqLongOpt100Implicit = new optWrapper.SeqLongOptJdbcType100
  protected implicit def seqLongOpt200Implicit = new optWrapper.SeqLongOptJdbcType200
  protected implicit def seqLongOpt500Implicit = new optWrapper.SeqLongOptJdbcType500
  protected implicit def seqLongOpt1000Implicit = new optWrapper.SeqLongOptJdbcType1000


  protected implicit def seqDbUUIDImplicit = new uuidWrapper.SeqDbUUIDJdbcTypeBase
  protected implicit def seqDbUUID100Implicit = new uuidWrapper.SeqDbUUIDJdbcType100
  protected implicit def seqDbUUID200Implicit = new uuidWrapper.SeqDbUUIDJdbcType200
  protected implicit def seqDbUUID500Implicit = new uuidWrapper.SeqDbUUIDJdbcType500
  protected implicit def seqDbUUID1000Implicit = new uuidWrapper.SeqDbUUIDJdbcType1000


  /**
    * Implicit evidence on how to write a placeholder
    *
    * @return
    */
  protected implicit def nonersJdbcType = new placeholderWrapper.PlaceholderJdbc


  /**
    * Implicit evidence on how to deal with binary arrays.  This is needed for uuid
    */
  private implicit val dbBinArrayJdbcType = (new profile.JdbcTypes).byteArrayJdbcType

  /**
    * Implicit evidence on how to deal with a dbuuid
    */
  private implicit val dbUUIDColumnType = profile.api.MappedColumnType.base[DbUUID, Array[Byte]](
    { uuid => uuid.binValue },
    { bin => DbUUID(bin) }
  )

  /**
    * Implicit class to allow filtering on sequence of uuid
    *
    * @param c  field
    * @param tt underlying field type implicitly
    * @tparam T underlying field type
    */
  implicit class EqAnyValuesHelper[T](c: profile.api.Rep[T])(implicit tt: TypedType[T]) {
    val any = SimpleFunction[String]("")
    type o = OptionMapperDSL.arg[T, T]

    /**
      * Perform an in statement for a sequence of uuids.  This is for compiled queries since you do not know how many
      * items at compile time.  The code creates an in statement with the input placeholder and 99 placeholders, which
      * is 100 items in total.
      *
      * @param toSplit sequence of uuids
      * @param om      some slick bs
      * @tparam R return type (boolean)
      * @return
      */
    def inSeqDbUUID100[R](toSplit: profile.api.Rep[BoundedSeq100[DbUUID]])(implicit om: o#to[Boolean, R]): Rep[R] = {
      inSeqDbUUIDLimit(toSplit, 100)
    }

    def inSeqDbUUID200[R](toSplit: profile.api.Rep[BoundedSeq200[DbUUID]])(implicit om: o#to[Boolean, R]): Rep[R] = {
      inSeqDbUUIDLimit(toSplit, 200)
    }

    def inSeqDbUUID500[R](toSplit: profile.api.Rep[BoundedSeq500[DbUUID]])(implicit om: o#to[Boolean, R]): Rep[R] = {
      inSeqDbUUIDLimit(toSplit, 500)
    }

    def inSeqDbUUID1000[R](toSplit: profile.api.Rep[BoundedSeq1000[DbUUID]])(implicit om: o#to[Boolean, R]): Rep[R] = {
      inSeqDbUUIDLimit(toSplit, 1000)
    }

    def inSeqDbUUIDLimit[R, S <: BoundedSeq[DbUUID]](
      toSplit: profile.api.Rep[S],
      limit: Int
    )(implicit om: o#to[Boolean, R]): Rep[R] = {
      toSplit match {
        case const: ConstColumn[_] =>
          // Constant list of ids
          val col = new ConstColumn[BoundedSeq[DbUUID]](const.toNode)
          // Create 100 fake placeholder parameters
          val fakes = (1 until limit).map {
            _ =>
              val placeKeeper = const.toNode.asInstanceOf[QueryParameter].copy(
                extractor = _ => new Placeholder(),
                buildType = nonersJdbcType
              )
              new ConstColumn[Placeholder](placeKeeper)
          }
          // Create in statement with fake placeholders
          om.column(
            Library.In,
            c.toNode,
            any(
              Seq(col) ++ fakes
            ).toNode
          )
        case _ =>
          // All other cases just use false which returns nothing
          om(LiteralColumn(false))
      }
    }

    def inSeqLongOpt100[R](
      toSplit: profile.api.Rep[BoundedSeq100[DbLongOptId]]
    )(implicit om: o#to[Boolean, R]): Rep[R] = {
      inSeqLongOptLimit(toSplit, 100)
    }

    def inSeqLongOpt200[R](
      toSplit: profile.api.Rep[BoundedSeq200[DbLongOptId]]
    )(implicit om: o#to[Boolean, R]): Rep[R] = {
      inSeqLongOptLimit(toSplit, 200)
    }

    def inSeqLongOpt500[R](
      toSplit: profile.api.Rep[BoundedSeq500[DbLongOptId]]
    )(implicit om: o#to[Boolean, R]): Rep[R] = {
      inSeqLongOptLimit(toSplit, 500)
    }

    def inSeqLongOpt1000[R](
      toSplit: profile.api.Rep[BoundedSeq1000[DbLongOptId]]
    )(implicit om: o#to[Boolean, R]): Rep[R] = {
      inSeqLongOptLimit(toSplit, 1000)
    }

    def inSeqLongOptLimit[R, S <: BoundedSeq[DbLongOptId]](
      toSplit: profile.api.Rep[S],
      limit: Int
    )(implicit om: o#to[Boolean, R]): Rep[R] = {
      toSplit match {
        case const: ConstColumn[_] =>
          // Constant list of ids
          val col = new ConstColumn[BoundedSeq[DbLongOptId]](const.toNode)
          // Create 100 fake placeholder parameters
          val fakes = (1 until limit).map {
            _ =>
              val placeKeeper = const.toNode.asInstanceOf[QueryParameter].copy(
                extractor = _ => new Placeholder(),
                buildType = nonersJdbcType
              )
              new ConstColumn[Placeholder](placeKeeper)
          }
          // Create in statement with fake placeholders
          om.column(
            Library.In,
            c.toNode,
            any(
              Seq(col) ++ fakes
            ).toNode
          )
        case _ =>
          // All other cases just use false which returns nothing
          om(LiteralColumn(false))
      }
    }
  }
}

