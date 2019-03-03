package co.actioniq.luna.dao


import slick.compiler.InsertCompiler.Mode
import slick.compiler.{CompilerState, Phase}
import slick.driver.JdbcProfile
import slick.jdbc.{H2Profile, JdbcProfile, MySQLProfile, PostgresProfile}
import slick.lifted.Tag
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
  import slick.SlickException
  import slick.ast._
  import slick.compiler.CompilerState
  import slick.util.ConstArray
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

trait CoolH2Profile extends H2Profile { self => JdbcProfile
  override lazy val updateCompiler = (compiler + new JdbcCodeGen(_.buildUpdate)).addAfter(new IgnoreUpdateCompiler(), Phase.hoistClientOps)
}

object CoolH2Profile extends CoolH2Profile

object CoolColumnOption {
  import slick.ast.ColumnOption
  case object IgnoreUpdate extends ColumnOption[Nothing]
}

case object IgnoreFieldForUpdate extends Mode {
  import slick.ast._
  def apply(fs: FieldSymbol) = !fs.options.contains(CoolColumnOption.IgnoreUpdate)
}


class IgnoreUpdateCompiler extends Phase {
  import slick.ast._
  override val name: String = "ignoreUpdateCompiler"

  override def apply(state: CompilerState): CompilerState = {
    var fields: ConstArray[FieldSymbol] = ConstArray.empty
    var fieldsToRemove: Map[Int, Boolean] = Map()
    state.map {
      case rsm @ ResultSetMapping(_, from, map) =>
        val newFrom = from match {
          case b @ Bind(sym, _, bindSelect) =>
            val newSelect = bindSelect match {
              case p @ Pure(pureSelect, _) =>
                val filteredSelect = pureSelect match {
                  case struct @ StructNode(elements) if elements.forall{ case (_, Select(Ref(str), _)) if str == sym => true; case _ => false} =>
                    fields = elements.map {
                      case (_, s @ Select(Ref(_), fieldInfo: FieldSymbol)) => fieldInfo
                    }
                    fieldsToRemove = getFieldsToRemove(fields)
                    struct.copy(elements=filterImportantFields(elements))
                  case n => n
                }
                p.copy(value=filteredSelect)
              case n => n
            }
            b.copy(select = newSelect).infer()
          case n => n
        }
        val newMap = map match {
          case typey @ TypeMapping(child, mapper, classTag) =>
            val newChild = child match {
              case pn @ ProductNode(children) =>
                val newChildren = children.toSeq.zipWithIndex.filter { row =>
                  !fieldsToRemove(row._2)
                }.map(_._1).zipWithIndex.map(_._1)
                pn.copy(children=ConstArray.from(newChildren))
              case n => n
            }
            val newMapper = mapper.copy(toBase = a => {
              val result = mapper.toBase(a)
              result match {
                case x: Product =>
                  tupleToFilteredTuple(x, fieldsToRemove)
                case n => n
              }
            })
            typey.copy(mapper = newMapper, child=newChild)
          case n => n
        }
        map match {
          case TypeMapping(_, _, _) => rsm.copy(from = newFrom, map = newMap)
          case n => rsm
        }
      case n => n
    }
  }

  def tupleToFilteredTuple(input: Product, fieldsToRemove: Map[Int, Boolean]): Product = {
    input.productIterator.zipWithIndex.filter{ row =>
      !fieldsToRemove(row._2)
    }.map(_._1).toList match {
      case List(a) => Tuple1(a)
      case List(a, b) => (a, b)
      case List(a, b, c) => (a, b, c)
      case List(a, b, c, d) => (a, b, c, d)
      case List(a, b, c, d, e) => (a, b, c, d, e)
      case List(a, b, c, d, e, f) => (a, b, c, d, e, f)
      case List(a, b, c, d, e, f, g) => (a, b, c, d, e, f, g)
      case List(a, b, c, d, e, f, g, h) => (a, b, c, d, e, f, g, h)
      case List(a, b, c, d, e, f, g, h, i) => (a, b, c, d, e, f, g, h, i)
      case List(a, b, c, d, e, f, g, h, i, j) => (a, b, c, d, e, f, g, h, i, j)
      case List(a, b, c, d, e, f, g, h, i, j, k) => (a, b, c, d, e, f, g, h, i, j, k)
      case List(a, b, c, d, e, f, g, h, i, j, k, l) => (a, b, c, d, e, f, g, h, i, j, k, l)
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m) => (a, b, c, d, e, f, g, h, i, j, k, l, m)
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
      case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
    }
    //v is 22
  }

  def filterImportantFields(elements: ConstArray[(TermSymbol, Node)]): ConstArray[(TermSymbol, Node)] = {
    elements.filter {
      case (_, s @ Select(Ref(_), fieldInfo: FieldSymbol)) => !fieldInfo.options.contains(CoolColumnOption.IgnoreUpdate)
    }
  }

  def getFieldsToRemove(fields: ConstArray[FieldSymbol]): Map[Int, Boolean] = {
    fields.zipWithIndex.map { rowWithIndex =>
      val isImportant = rowWithIndex._1.options.contains(CoolColumnOption.IgnoreUpdate)
      (rowWithIndex._2, isImportant)
    }.toMap
  }
}