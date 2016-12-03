package mpmens

import mpmens.model.Component
import org.chocosolver.solver.constraints.nary.cnf.{ILogical, LogOp}
import org.chocosolver.solver.variables.IntVar

import scala.collection.mutable

trait RolesMixin {
  this: System =>

  /** Represents a role in an ensemble. Implements methods to build membership over components contained in a role. */
  class Role[ComponentType <: Component](val name: String, private[mpmens] val items: Items[ComponentType]) {
    private[mpmens] val membershipVar = model.setVar(name, Array.empty[Int], 0 until items.size toArray)

    items.mapChildToParent(this)

    class Cardinality {
      def is(num: Int) = LogicalBoolVar(model.arithm(membershipVar.getCard, "=", num).reify())
    }

    def cardinality: Cardinality = new Cardinality

    def sum(fun: ComponentType => Int): IntVar = {
      val sumVar = model.intVar(IntMinValue, IntMaxValue)
      model.sumElements(membershipVar, items.components.map(fun), sumVar).post()
      sumVar
    }

    private def getImplies(fun: ComponentType => Logical, combinator: Seq[ILogical] => LogOp) = {
      val clauses = mutable.ListBuffer.empty[ILogical]

      for (idx <- 0 until items.size) {
        fun(items.components(idx)) match {
          case LogicalBoolean(value) => if (!value) clauses += model.notMember(idx, membershipVar).reify
          case LogicalBoolVar(value) => clauses += LogOp.implies(model.member(idx, membershipVar).reify, value)
          case LogicalLogOp(value) => clauses += LogOp.implies(model.member(idx, membershipVar).reify, value)
          case LogicalEmpty() =>
        }
      }

      if (clauses.size > 0)
        LogicalLogOp(combinator(clauses))
      else
        LogicalEmpty()
    }

    def all(fun: ComponentType => Logical): LogicalClause = getImplies(fun, LogOp.and(_ : _*))

    def some(fun: ComponentType => Logical): LogicalClause = getImplies(fun, LogOp.or(_ : _*))

    private def selectedItems = {
      import scala.collection.JavaConverters._
      for (idx <- membershipVar.getValue.asScala) yield items.components(idx)
    }

    override def toString(): String = s"""Role "$name":\n""" + indent(selectedItems.mkString("\n"), 1)
  }
}
