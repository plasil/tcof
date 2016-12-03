package mpmens

import mpmens.model.Component
import org.chocosolver.solver.variables.IntVar

import scala.collection.mutable

trait EnsemblesMixin {
  this: System =>

  class Ensemble[AnchorType <: Component](val anchor: AnchorType) {
    private val roles = mutable.Map.empty[String, Role[_]]
    private[System] var utilityVar: IntVar = null

    def role[ComponentType <: Component](name: String, items: Items[ComponentType]) = {
      val role = new Role[ComponentType](name, items)
      roles += (name -> role)
      role
    }

    def role[ComponentType <: Component](name: String) = roles(name).asInstanceOf[Role[ComponentType]]

    def utility(cst: IntVar) {
      utilityVar = cst
    }

    /* TODO, add variable that indicates if the ensemble is instantiated and condition the ensure clauses below and the cost by this variable */

    def membership(clauses: LogicalClause*): Unit = {
      for (clause <- clauses) {
        clause match {
          case LogicalBoolVar(value) => model.addClauseTrue(value)
          case LogicalLogOp(value) => model.addClauses(value)
          case LogicalEmpty() =>
        }
      }
    }

    def utility: Int =
      if (utilityVar != null && utilityVar.isInstantiated)
        utilityVar.getValue
      else
        0

    override def toString(): String = s"Ensemble:\n" + "  Anchor:\n" + indent(anchor.toString, 2) + "\n" + indent(roles.values.mkString("\n"), 1)
  }


}
