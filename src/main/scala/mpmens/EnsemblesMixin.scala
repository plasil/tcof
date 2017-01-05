package mpmens

import mpmens.Utils._

import scala.collection.mutable

trait EnsemblesMixin {
  this: Universe =>

  trait Ensemble extends WithName with WithUtility with WithEnsembleGroups with WithRoles with WithActionsInEnsemble with Initializable {
    private[mpmens] val _membershipClauseFuns = mutable.ListBuffer.empty[() => Logical]

    def membership(clause: => Logical): Unit = {
      _membershipClauseFuns += clause _
    }

    private[mpmens] def _buildEnsembleClause: Logical = {
      if (_membershipClauseFuns.nonEmpty)
        _buildEnsembleGroupClause && LogicalUtils.and(_membershipClauseFuns.map(_.apply()))
      else _buildEnsembleGroupClause
    }

    override def toString: String =
      s"""Ensemble "$name" (utility: $solutionUtility):\n${indent(_roles.values.mkString(""), 1)}${indent(_ensembleGroups.mkString(""), 1)}"""
  }

}
