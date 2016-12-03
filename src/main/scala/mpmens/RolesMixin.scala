package mpmens

import mpmens.model.Component
import org.chocosolver.solver.constraints.nary.cnf.{ILogical, LogOp}
import org.chocosolver.solver.variables.{IntVar, SetVar}

import scala.collection.mutable

trait RolesMixin {
  this: System =>

  /** Represents a role in an ensemble. Implements methods to build membership over components contained in a role. */
  class Role[ComponentType <: Component](val name: String, private[mpmens] val items: Members[ComponentType])
      extends WithSolverModelFromSystem with WithMembers[ComponentType] {

    private[mpmens] val allMembersVar = solverModel.setVar(name, Array.empty[Int], 0 until items.size toArray)

    override def allMembers = items.components

    items.mapChildToParent(this)

    private def getImplies(fun: ComponentType => Logical, combinator: Seq[ILogical] => LogOp, emptyBehavior: Boolean) = {
      val clauses = mutable.ListBuffer.empty[ILogical]

      for (idx <- 0 until items.size) {
        fun(items.components(idx)) match {
          case LogicalBoolean(value) => if (!value) clauses += solverModel.notMember(idx, allMembersVar).reify
          case LogicalBoolVar(value) => clauses += LogOp.implies(solverModel.member(idx, allMembersVar).reify, value)
          case LogicalLogOp(value) => clauses += LogOp.implies(solverModel.member(idx, allMembersVar).reify, value)
        }
      }

      if (clauses.size > 0)
        LogicalLogOp(combinator(clauses))
      else
        LogicalBoolean(emptyBehavior)
    }

    def all(fun: ComponentType => Logical): Logical = getImplies(fun, LogOp.and(_ : _*), true)

    def some(fun: ComponentType => Logical): Logical = getImplies(fun, LogOp.or(_ : _*), false)

    private def selectedItems = {
      import scala.collection.JavaConverters._
      for (idx <- allMembersVar.getValue.asScala) yield items.components(idx)
    }

    override def toString(): String =
      s"""Role "$name"
         |${indent(selectedItems.mkString("\n"), 1)}
       """.stripMargin
  }
}
