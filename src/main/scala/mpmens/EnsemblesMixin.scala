package mpmens

import org.chocosolver.solver.constraints.nary.cnf.{ILogical, LogOp}
import org.chocosolver.solver.variables.{IntVar, SetVar}

import scala.collection.mutable

trait EnsemblesMixin {
  this: System =>

  class Ensemble[AnchorType <: Component](val name: String, val anchor: AnchorType) extends WithUtility with WithEnsembleGroups with WithRoles {
    private[mpmens] var membershipClause: Logical = null

    def membership(clause: Logical): Unit = {
      membershipClause = clause
    }

    private[mpmens] def ensembleClause: Logical =
      if (membershipClause != null)
        ensembleGroupClause && membershipClause
      else ensembleGroupClause

    override def toString(): String =
      s"""Ensemble "$name" (utility: ${solutionUtility}):""" +
      (if (anchor != null)
        s"""
           |  Anchor:
           |${indent(anchor.toString, 2)}
           |""".stripMargin
      else
        "\n"
      ) +
      s"""${indent(roles.values.mkString("\n"), 1)}${indent(ensembleGroups.mkString("\n"), 1)}"""

  }

}
