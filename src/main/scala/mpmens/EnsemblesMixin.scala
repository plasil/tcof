package mpmens

trait EnsemblesMixin {
  this: Universe =>

  class Ensemble(val name: String) extends WithUtility with WithEnsembleGroups with WithRoles with WithActionsInEnsemble {
    private[mpmens] var membershipClause: Logical = _

    def membership(clause: Logical): Unit = {
      membershipClause = clause
    }

    private[mpmens] def ensembleClause: Logical =
      if (membershipClause != null)
        ensembleGroupClause && membershipClause
      else ensembleGroupClause

    override def toString: String =
      s"""Ensemble "$name" (utility: $solutionUtility):\n${indent(roles.values.mkString(""), 1)}${indent(ensembleGroups.mkString(""), 1)}"""
  }

}
