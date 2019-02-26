package co.actioniq.luna.dao


import slick.ast.{Bind, CompiledStatement, FieldSymbol, Filter, Node, ProductNode, Pure, Ref, ResultSetMapping, Select, StructNode}
import slick.compiler.InsertCompiler.Mode
import slick.compiler.{CompilerState, Phase}
import slick.dbio.{Effect, NoStream}
import slick.jdbc.{H2Profile, JdbcProfile, JdbcResultConverterDomain, MySQLProfile, PostgresProfile}
import slick.lifted.Tag
import slick.relational.{CompiledMapping, ProductResultConverter, TypeMappingResultConverter}
import slick.sql.FixedSqlAction
import slick.util.ConstArray


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
      val newN = n match {
        case c @ Comprehension(sym, from: TableNode, Pure(select, selectIdentity), where, None, _, None, None, None, None, false) =>
          println(s"WEEEE comprehension $select $selectIdentity")
          val newSelect = select match {
            case f @ Select(Ref(struct), _) if struct == sym =>
              println(s"WEEEE select $f")
              f
            case ProductNode(ch) if ch.forall { case Select(Ref(struct), _) if struct == sym => true; case _ => false } =>
              val newChildren = ConstArray.from(ch.toSeq.filter {
                case Select(Ref(s), fs: FieldSymbol) => IgnoreFieldForUpdate(fs)
                case _: Node => true
              })
              println(s"WEEE product $ch")
              println(s"WEEE product $newChildren")
              ProductNode(ch)
            case n: Node => n
          }
          c.copy(select = Pure(newSelect, selectIdentity))
        case n: Node => n
      }
      super.expr(newN, skipParens)
    }

    override def buildUpdate = {
      val (gen, from, where, select) = tree match {
        case Comprehension(sym, from: TableNode, Pure(select, _), where, None, _, None, None, None, None, false) =>

          println(s"Some select $select")
          select match {
            case f @ Select(Ref(struct), _) if struct == sym =>
              (sym, from, where, ConstArray(f.field))
            case ProductNode(ch) if ch.forall{ case Select(Ref(struct), _) if struct == sym => true; case _ => false} =>
              println(s"Product node $ch")
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
  override lazy val updateCompiler = (compiler + new JdbcCodeGen(_.buildUpdate)).addAfter(new IgnoreUpdateCompiler(), Phase.flattenProjections)

  override def createUpdateActionExtensionMethods[T](tree: Node, param: Any): UpdateActionExtensionMethods[T] = {
    println(s"MEOW")
    new CoolUpdate[T](tree, param)
  }
  class CoolUpdate[T](tree: Node, param: Any) extends UpdateActionExtensionMethodsImpl[T](tree, param) {
    override def update(value: T): FixedSqlAction[Int, NoStream, Effect.Write] = {
      println(s"VAL ${value}")
      println(s"DUMP ${converter.getDumpInfo}")
      println(tree.children)
      tree.children.foreach { child =>
        println(s"Child $child")
        child match {
          case CompiledMapping(conv, tpe) =>
            println(s"CONV $conv")
            println(s"type $tpe")
          case c => c
        }
      }
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

case object IgnoreFieldForUpdate extends Mode {
  def apply(fs: FieldSymbol) = !fs.options.contains(CoolColumnOption.IgnoreUpdate)
}

class IgnoreUpdateCompiler extends Phase {
  override val name: String = "ignoreUpdateCompiler"

  override def apply(state: CompilerState): CompilerState = {
    println("APPLYZ")
    val newState = state.map { tree =>
      tree match {
        case b@Bind(something, from, to) =>
          println(s"From $from")
          from match {
            case Filter(s, fFrom, fTo) =>
              println(s"Filter $fFrom $fTo")
          }
          println(s"To $to")
          val newTo = to match {
            case Pure(v, i) =>
              println(s"Pure $v $i")
              val newNode = v match {
                case s @ StructNode(children) =>
                  children.foreach { child =>
                    println(s"Child $child")
                  }
                  val newChildren = children.filter {
                    case (_, Select(Ref(_), fs: FieldSymbol)) => IgnoreFieldForUpdate(fs)
                    case _: Node => true
                  }
                  s.copy(elements = children)
              }
              println(s"Pure $newNode $i")
              Pure(newNode, i)
            case n: Node => n
          }
          println(s"To $newTo")
          b.copy(select = newTo)
        case n: Node => n
      }
    }
    println(s"NEW $newState")
    newState
  }
  /*
  override def apply(state: CompilerState): CompilerState = {
    println("APPLY")
    val newState = state.map { tree =>
      tree match {
        case b @ Bind(something, from, to) =>
          println(s"From $from")
          from match {
            case Filter(s, fFrom, fTo) =>
              println(s"Filter $fFrom $fTo")
          }
          println(s"To $to")
          val newTo = to match {
            case Pure(v, i) =>
              println(s"Pure $v $i")
              val newNode = v match {
                case ProductNode(children) =>
                  println(s"ProductNode $children")
                  val filteredChildren = ConstArray.from(children.filter {
                    case Select(Ref(s), fs: FieldSymbol) => IgnoreFieldForUpdate(fs)
                    case _: Node => true
                  }.toSeq)
                  println(s"New children ${filteredChildren}")
                  println(s"New product node ${ProductNode(filteredChildren)}")
                  ProductNode(children)
                case n: Node => n
              }
              println(s"Pure $newNode $i")
              Pure(newNode, i)
            case n: Node => n
          }
          println(s"To $newTo")
          b.copy(select = newTo)
        case n: Node => n
      }
    }
    println(s"NEW $newState")
    newState
  }
  */
}