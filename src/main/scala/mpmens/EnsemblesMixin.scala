package mpmens

import org.chocosolver.solver.constraints.nary.cnf.{ILogical, LogOp}
import org.chocosolver.solver.variables.{IntVar, SetVar}

import scala.collection.mutable

trait EnsemblesMixin {
  this: Universe =>

  class Ensemble(val name: String) extends WithUtility with WithEnsembleGroups with WithRoles {
    private[mpmens] var membershipClause: Logical = null

    def membership(clause: Logical): Unit = {
      membershipClause = clause
    }

    private[mpmens] def ensembleClause: Logical =
      if (membershipClause != null)
        ensembleGroupClause && membershipClause
      else ensembleGroupClause

    override def toString(): String =
      s"""Ensemble "$name" (utility: ${solutionUtility}):\n${indent(roles.values.mkString(""), 1)}${indent(ensembleGroups.mkString(""), 1)}"""
  }

}
