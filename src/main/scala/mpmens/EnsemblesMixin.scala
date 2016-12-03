package mpmens

import mpmens.model.Component
import org.chocosolver.solver.constraints.nary.cnf.{ILogical, LogOp}
import org.chocosolver.solver.variables.{IntVar, SetVar}

import scala.collection.mutable

trait EnsemblesMixin {
  this: System =>

  class Ensemble[AnchorType <: Component](val name: String, val anchor: AnchorType) {
    private val roles = mutable.Map.empty[String, Role[_]]
    private var utilityVar: IntVar = null
    private[EnsemblesMixin] var membershipClause: Logical = null

    def role[ComponentType <: Component](name: String, items: Members[ComponentType]) = {
      val role = new Role[ComponentType](name, items)
      roles += (name -> role)
      role
    }

    def role[ComponentType <: Component](name: String) = roles(name).asInstanceOf[Role[ComponentType]]

    def solutionUtility(cst: IntVar) {
      utilityVar = cst
    }

    /* TODO, add variable that indicates if the ensemble is instantiated and condition the ensure clauses below and the cost by this variable */

    def membership(clause: Logical): Unit = {
      membershipClause = clause
    }

    def solutionUtility: Int =
      if (utilityVar != null && utilityVar.isInstantiated)
        utilityVar.getValue
      else
        0

    override def toString(): String =
      s"""Ensemble "$name" (utility: ${solutionUtility}):
         |  Anchor: ${indent(anchor.toString, 2)}
         |${indent(roles.values.mkString("\n"), 1)}
         |""".stripMargin
  }

  class EnsembleGroup[EnsembleType <: Ensemble[_]](private val generator: () => Array[EnsembleType]) extends WithSolverModelFromSystem with WithMembers[EnsembleType] {
    private[mpmens] var allMembersVar: SetVar = null

    private[mpmens] var ensembles: Array[EnsembleType] = null

    private[mpmens] var membershipClause: Logical = null

    override def allMembers = ensembles

    private[mpmens] def reset(): Unit = {
      ensembles = generator()
      allMembersVar = solverModel.setVar(Array.empty[Int], 0 until ensembles.size toArray)

      val clauses = mutable.ListBuffer.empty[ILogical]

      for (idx <- 0 until ensembles.size) {
        val ensemble = ensembles(idx)

        ensemble.membershipClause match {
          case LogicalBoolean(value) => if (!value) clauses += solverModel.notMember(idx, allMembersVar).reify
          case LogicalBoolVar(value) => clauses += LogOp.implies(solverModel.member(idx, allMembersVar).reify, value)
          case LogicalLogOp(value) => clauses += LogOp.implies(solverModel.member(idx, allMembersVar).reify, value)
        }
      }

      membershipClause = if (clauses.size > 0)
        LogicalLogOp(LogOp.and(clauses : _*))
      else
        LogicalBoolean(true)
    }


  }


}
