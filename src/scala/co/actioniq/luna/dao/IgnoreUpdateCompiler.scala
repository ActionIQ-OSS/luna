package co.actioniq.luna.dao

import slick.SlickException
import slick.compiler.InsertCompiler.Mode
import slick.compiler.{CompilerState, Phase}
import slick.util.ConstArray

/**
  * The goal of this compile stage is to remove fields from update statements that should not be updated when using
  * case classes.  This is useful for updating using a case class and ignoring indexed fields, which would cause a
  * deadlock.  This only removes fields from updating using a case class, sqlu and updating specific fields
  * are not affected by this compiler.
  *
  * How it works:
  * 1. Assumes ResultSetMapping state (probably after hoistClientOps).
  * 2. Looks for fields in "from" with column option of IgnoreFieldForUpdate.  Removes those.
  * 3. Removes from mapper fields with same ordinal position as fields in #2.
  * 4. Creates a new "wrapper" function around mapper (mapper function goes from case class to tuple)
  * that will create a tuple without ignored fields.
  */
class IgnoreUpdateCompiler extends Phase {
  import slick.ast._
  override val name: String = "ignoreUpdateCompiler"

  def allElementsInTable(elements: ConstArray[(TermSymbol, Node)], table: TermSymbol): Boolean = {
    elements.forall{
      case (_, Select(Ref(str), _)) if str == table => true
      case _ => false
    }
  }

  override def apply(state: CompilerState): CompilerState = {
    var fieldsToRemove: Map[String, Boolean] = Map()
    var positionalFieldsToKeep: Set[Int] = Set()
    state.map {
      case rsm @ ResultSetMapping(_, from, map) =>
        val newFrom = from match {
          case b @ Bind(sym, _, bindSelect) =>
            val newSelect = bindSelect match {
              case p @ Pure(pureSelect, _) =>
                val filteredSelect = pureSelect match {
                  case struct @ StructNode(elements) if allElementsInTable(elements, sym) =>
                    val fields = elements.map {
                      case (t, Select(Ref(_), fieldInfo: FieldSymbol)) =>
                        (t.toString(), fieldInfo)
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
          case typey @ TypeMapping(child, mapper, _) =>
            val newChild = child match {
              case pn @ ProductNode(children) =>
                val newChildren = children.toSeq.zipWithIndex.collect {
                  case (p @ Path(elements), index: Int) if elements.nonEmpty && !fieldsToRemove.getOrElse(elements.head.toString(), false) =>
                    positionalFieldsToKeep += index
                    p
                }
                pn.copy(children=ConstArray.from(newChildren))
              case n => n
            }
            val newMapper = mapper.copy(toBase = a => {
              val result = mapper.toBase(a)
              result match {
                case x: Product =>
                  tupleToFilteredTuple(x, positionalFieldsToKeep)
                case n => n
              }
            })
            typey.copy(mapper = newMapper, child=newChild)
          case n => n
        }
        map match {
          case TypeMapping(_, _, _) => rsm.copy(from = newFrom, map = newMap).infer()
          case n => rsm
        }
      case n => throw new SlickException(s"Expected ResultSetMapping for IgnoreUpdateCompiler, got $n")
    }
  }

  def tupleToFilteredTuple(input: Product, positionalFieldsToKeep: Set[Int]): Product = {
    new Product {
      private val items = input.productIterator.zipWithIndex.collect{
        case row: (Any, Int) if positionalFieldsToKeep(row._2) => row._1
      }.toList
      override def productElement(n: Int): Any = items(n)

      override def productArity: Int = items.size

      override def canEqual(that: Any): Boolean = that.isInstanceOf[Product]
    }
  }

  def filterImportantFields(elements: ConstArray[(TermSymbol, Node)]): ConstArray[(TermSymbol, Node)] = {
    elements.filter {
      case (_, Select(Ref(_), fieldInfo: FieldSymbol)) => !fieldInfo.options.contains(ExtraColumnOption.IgnoreUpdate)
    }
  }

  def getFieldsToRemove(fields: ConstArray[(String, FieldSymbol)]): Map[String, Boolean] = {
    fields.map { fieldWithTermSymbol =>
      val isImportant = fieldWithTermSymbol._2.options.contains(ExtraColumnOption.IgnoreUpdate)
      (fieldWithTermSymbol._1, isImportant)
    }.toMap
  }
}

object ExtraColumnOption {
  import slick.ast.ColumnOption
  case object IgnoreUpdate extends ColumnOption[Nothing]
}

case object IgnoreFieldForUpdate extends Mode {
  import slick.ast._
  def apply(fs: FieldSymbol) = !fs.options.contains(ExtraColumnOption.IgnoreUpdate)
}
