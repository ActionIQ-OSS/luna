package co.actioniq.luna

import co.actioniq.luna.dao.{DbLongOptId, DbUUID}
import slick.SlickException
import slick.ast._
import slick.ast.ScalaBaseType._
import slick.lifted._

// scalastyle:off
/**
  * A wrapper around slick Rep option to allow easy comparisons of optional fields to optional values.
  * Example: column in table team_id is nullable and context.teamId is an option.  We want to be able to do
  * .filter(row => row.teamId === context.teamId)
  * however this does not work if context.teamId is None because the SQL resolves to where team_id = null instead of
  * where team_id is null.
  * This class adds a function =?= to compare two options and if the right param is None the sql will turn into "is null"
  * =:= on the other hand will compare two options and return true if they are equal or the input is empty
  * @param c
  * @tparam B1
  */
class OptionCompareOption[B1](val c: Rep[Option[B1]]) extends AnyVal with ColumnExtensionMethods[B1, Option[B1]]
  with OptionExtensionMethods[B1] {
  /** Get the value inside this Option, if it is non-empty, otherwise throw a SlickException. This
    * operation is only allowed in places where it can be performed at the client side (e.g. not
    * inside a subquery that cannot be fused), otherwise the exception is thrown during query
    * compilation. */
  def get: Rep[B1] =
    Rep.forNode[B1](GetOrElse(c.toNode, () =>
      throw new dao.DAOException("Read NULL value for column " + this)))(c
      .asInstanceOf[Rep.TypedRep[_]].tpe.asInstanceOf[OptionType].elementType.asInstanceOf[TypedType[B1]])

  /**
    * Compare two options, if the input is empty convert the sql to "field is null"
    * @param e
    * @param om
    * @tparam P2
    * @tparam R
    * @return
    */
  def =?= [P2, R](e: Rep[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]): Rep[R] = {
    val classes = e match {
      case p: Product =>
        p.productIterator.filter(cls => !cls.isInstanceOf[Node] && cls.isInstanceOf[Option[_]]).toSeq
      case _ => Seq()
    }
    classes.headOption match {
      case Some(cls) if cls.asInstanceOf[Option[_]].isEmpty => new AnyOptionExtensionMethods[Rep[Option[B1]], R](c).isEmpty
      case _ =>
        om.column(Library.==, n, e.toNode)
    }
  }

  /**
    * Compare two options, if the input is empty then don't add a filter / filter on true
    * @param e
    * @param om
    * @tparam P2
    * @tparam R
    * @return
    */
  def =:= [P2, R](e: Rep[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]): Rep[R] = {
    val classes = e match {
      case p: Product =>
        p.productIterator.filter(cls => !cls.isInstanceOf[Node] && cls.isInstanceOf[Option[_]]).toSeq
      case _ => Seq()
    }
    classes.headOption match {
      case Some(cls) if cls.asInstanceOf[Option[_]].isEmpty => LiteralColumn(true).asInstanceOf[Rep[R]]
      case _ =>
        om.column(Library.==, n, e.toNode)
    }
  }
}

object OptionCompareOption{
  def optionCompare[B1](c: Rep[Option[B1]]): OptionCompareOption[B1] = new OptionCompareOption(c)
  def optLongCompare(c: Rep[DbLongOptId]): OptLongCompare = new OptLongCompare(c)
  def uuidCompare(c: Rep[DbUUID]): UUIDCompare = new UUIDCompare(c)
}


class OptLongCompare(val c: Rep[DbLongOptId]) extends AnyVal
  with BaseExtensionMethods[DbLongOptId] {

  def equalsLong [R](e: Rep[Long]) ={
    column(Library.==, n, e.toNode)
  }

  def column(fs: FunctionSymbol, ch: Node*)(implicit bt: TypedType[DbLongOptId]): Rep[Option[Boolean]] = {
    implicit val tt = b1Type.asInstanceOf[TypedType[Option[Long]]]
    Rep.forNode[Option[Boolean]](fs.typed(tt, ch: _*))
  }
}

class UUIDCompare(val c: Rep[DbUUID]) extends AnyVal
  with BaseExtensionMethods[DbUUID] {

  def equalsArrayByte [R](e: Rep[Array[Byte]]) ={
    column(Library.==, n, e.toNode)
  }

  def column(fs: FunctionSymbol, ch: Node*)(implicit bt: TypedType[DbUUID]): Rep[Option[Boolean]] = {
    implicit val tt = b1Type.asInstanceOf[TypedType[Array[Byte]]]
    Rep.forNode[Option[Boolean]](fs.typed(tt, ch: _*))
  }

}

// scalastyle:on

