package co.actioniq.luna.dao


import slick.dbio.{Effect, NoStream}
import slick.jdbc.{H2Profile, JdbcProfile, JdbcResultConverterDomain, MySQLProfile, PostgresProfile}
import slick.lifted.Tag
import slick.sql.FixedSqlAction


/**
  * A table that has an IDType for an ID
 *
  * @tparam I IDType
  */
trait IdTable[I <: IdType]{
  def id: slick.lifted.Rep[I]
}

object DAOTable {
  type Table[V <: IdModel[I], I <: IdType, P <: JdbcProfile] = P#Table[V] with IdTable[I]
}

abstract class MySQLDAOTable[V <: IdModel[I], I <: IdType](
  tag: Tag,
  tableName: String,
  schemaName: Option[String] = None
) extends CoolMySQLProfile.Table[V](tag, schemaName, tableName)
  with IdTable[I]
  with JdbcTypeImplicits.mySQLJdbcTypeImplicits.DbImplicits {
  self: DAOTable.Table[V, I, CoolMySQLProfile] =>
}



abstract class PostgresDAOTable[V <: IdModel[I], I <: IdType](
  tag: Tag,
  tableName: String,
  schemaName: Option[String] = None
) extends PostgresProfile.Table[V](tag, schemaName, tableName)
  with IdTable[I]
  with JdbcTypeImplicits.postgresJdbcTypeImplicits.DbImplicits {
  self: DAOTable.Table[V, I, PostgresProfile] =>
}

abstract class H2DAOTable[V <: IdModel[I], I <: IdType](
  tag: Tag,
  tableName: String,
  schemaName: Option[String] = None
) extends CoolH2Profile.Table[V](tag, schemaName, tableName)
  with IdTable[I]
  with JdbcTypeImplicits.h2JdbcTypeImplicits.DbImplicits {
  self: DAOTable.Table[V, I, CoolH2Profile] =>
}

class DAODynamicProfileTable[P <: JdbcProfile] (
  val profile: P,
  val implicits: JdbcTypeImplicits[P]
) {
  abstract class DAOTable[V <: IdModel[I], I <: IdType](
    tag: Tag,
    tableName: String,
    schemaName: Option[String] = None
  ) extends profile.Table[V](tag, schemaName, tableName)
    with IdTable[I]
    with implicits.DbImplicits
}

trait CoolMySQLProfile extends MySQLProfile {
  import slick.ast._
  import slick.util.ConstArray
  import slick.SlickException
  import slick.compiler.CompilerState
  import slick.util.MacroSupport.macroSupportInterpolation
  class OtherQueryBuilder(override val tree: Node, override val state: CompilerState) extends QueryBuilder(tree, state) {
    override def buildUpdate = {
      val (gen, from, where, select) = tree match {
        case Comprehension(sym, from: TableNode, Pure(select, _), where, None, _, None, None, None, None, false) => select match {
          case f @ Select(Ref(struct), _) if struct == sym => (sym, from, where, ConstArray(f.field))
          case ProductNode(ch) if ch.forall{ case Select(Ref(struct), _) if struct == sym => true; case _ => false} =>
            (sym, from, where, ch.map{ case Select(Ref(_), field) => field })
          case _ => throw new SlickException("A query for an UPDATE statement must select table columns only -- Unsupported shape: "+select)
        }
        case o => throw new SlickException("A query for an UPDATE statement must resolve to a comprehension with a single table -- Unsupported shape: "+o)
      }

      val qtn = quoteTableName(from)
      symbolName(gen) = qtn // Alias table to itself because UPDATE does not support aliases
      b"update $qtn set "
      b.sep(select, ", ")(field => b += symbolName(field) += " = ?")
      if(!where.isEmpty) {
        b" where "
        expr(where.reduceLeft((a, b) => Library.And.typed[Boolean](a, b)), true)
      }
      b.build
    }
  }

  override def createQueryBuilder(n: slick.ast.Node, state: slick.compiler.CompilerState): QueryBuilder = new OtherQueryBuilder(n, state)
  override lazy val updateCompiler = compiler + new JdbcCodeGen(_.buildUpdate)
}

object CoolMySQLProfile extends CoolMySQLProfile {
  import slick.ast._
  final case class RowNum(sym: AnonSymbol, inc: Boolean) extends NullaryNode with SimplyTypedNode {
    type Self = RowNum
    def buildType = ScalaBaseType.longType
    def rebuild = copy()
  }

  final case class RowNumGen(sym: AnonSymbol, init: Long) extends NullaryNode with SimplyTypedNode {
    type Self = RowNumGen
    def buildType = ScalaBaseType.longType
    def rebuild = copy()
  }
}

trait CoolH2Profile extends H2Profile {
  import slick.ast._
  import slick.util.ConstArray
  import slick.SlickException
  import slick.compiler.CompilerState
  import slick.util.MacroSupport.macroSupportInterpolation
  import slick.relational.TypeMappingResultConverter
  class OtherQueryBuilder(override val tree: Node, override val state: CompilerState) extends QueryBuilder(tree, state) {

    override def expr(n: Node, skipParens: Boolean): Unit = {
      println(s"Mooo $n")
      super.expr(n, skipParens)
    }

    override def buildUpdate = {
      val (gen, from, where, select) = tree match {
        case Comprehension(sym, from: TableNode, Pure(select, _), where, None, _, None, None, None, None, false) => select match {
          case f @ Select(Ref(struct), _) if struct == sym => (sym, from, where, ConstArray(f.field))
          case ProductNode(ch) if ch.forall{ case Select(Ref(struct), _) if struct == sym => true; case _ => false} =>
            (sym, from, where, ch.map{ case Select(Ref(_), field) => field })
          case _ => throw new SlickException("A query for an UPDATE statement must select table columns only -- Unsupported shape: "+select)
        }
        case o => throw new SlickException("A query for an UPDATE statement must resolve to a comprehension with a single table -- Unsupported shape: "+o)
      }

      println(s"boom boom")
      val selectsForUpdate = select.filter {
        case s: FieldSymbol => !s.options.contains(CoolColumnOption.IgnoreUpdate)
        case _ => true
      }
      val qtn = quoteTableName(from)
      symbolName(gen) = qtn // Alias table to itself because UPDATE does not support aliases
      b"update $qtn set "
      b.sep(selectsForUpdate, ", ")(field => b += symbolName(field) += " = ?")
      if(!where.isEmpty) {
        b" where "
        expr(where.reduceLeft((a, b) => Library.And.typed[Boolean](a, b)), true)
      }
      val result = b.build
      println(s"MOOM MOOM ${result}")
      result
    }
  }

  override def createQueryBuilder(n: slick.ast.Node, state: slick.compiler.CompilerState): QueryBuilder = new OtherQueryBuilder(n, state)
  override lazy val updateCompiler = compiler + new JdbcCodeGen(_.buildUpdate)

  override def createUpdateActionExtensionMethods[T](tree: Node, param: Any): UpdateActionExtensionMethods[T] = {
    println(s"MEOW")
    new CoolUpdate[T](tree, param)
  }
  class CoolUpdate[T](tree: Node, param: Any) extends UpdateActionExtensionMethodsImpl[T](tree, param) {
    override def update(value: T): FixedSqlAction[Int, NoStream, Effect.Write] = {
      println(s"VAL ${value}")
      println(s"DUMP ${converter.getDumpInfo}")
      println(tree.children)
      println(converter.asInstanceOf[TypeMappingResultConverter[JdbcResultConverterDomain, T, _]].toBase(value))
      println(s"HMMM ${converter.asInstanceOf[TypeMappingResultConverter[JdbcResultConverterDomain, T, _]].child.getDumpInfo}")
      super.update(value)
    }
  }
}

object CoolH2Profile extends CoolH2Profile

object CoolColumnOption {
  import slick.ast.ColumnOption
  case object IgnoreUpdate extends ColumnOption[Nothing]
}
