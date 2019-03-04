package co.actioniq.luna.dao

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
          case typey @ TypeMapping(child, mapper, _) =>
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
    new Product {
      private val items = input.productIterator.zipWithIndex.filter{ row =>
        !fieldsToRemove(row._2)
      }.map(_._1).toList
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

  def getFieldsToRemove(fields: ConstArray[FieldSymbol]): Map[Int, Boolean] = {
    fields.zipWithIndex.map { rowWithIndex =>
      val isImportant = rowWithIndex._1.options.contains(ExtraColumnOption.IgnoreUpdate)
      (rowWithIndex._2, isImportant)
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
