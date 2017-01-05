package mpmens

import mpmens.Utils._

import scala.collection.mutable

trait EnsemblesMixin {
  this: Universe =>

  trait Ensemble extends WithConfig with WithName with WithUtility with WithEnsembleGroups with WithRoles with WithActionsInEnsemble with CommonImplicits with Initializable {
    private[mpmens] val _membershipClauseFuns = mutable.ListBuffer.empty[() => Logical]

    def membership(clause: => Logical): Unit = {
      _membershipClauseFuns += clause _
    }

    private[mpmens] def _buildEnsembleClause: Logical = {
      if (_membershipClauseFuns.nonEmpty)
        _buildEnsembleGroupClause && _solverModel.and(_membershipClauseFuns.map(_.apply()))
      else _buildEnsembleGroupClause
    }

    override def toString: String =
      s"""Ensemble "$name" (utility: $solutionUtility):\n${indent(_roles.values.mkString(""), 1)}${indent(_ensembleGroups.mkString(""), 1)}"""


    implicit def iterableToMembersStatic[ComponentType <: Universe#Component](components: Iterable[ComponentType]): RoleMembersStatic[ComponentType] = new RoleMembersStatic(components)
    implicit def roleToMembersCond[ComponentType <: Universe#Component](role: Role[ComponentType]): RoleMembersCond[ComponentType] = new RoleMembersCond(role)
    implicit def roleToMembersEquiv[ComponentType <: Universe#Component](role: Role[ComponentType]): RoleMembersEquiv[ComponentType] = new RoleMembersEquiv(role)
    implicit def ensembleGroupToMembers[EnsembleType <: Ensemble](group: EnsembleGroup[EnsembleType]): EnsembleGroupMembers[EnsembleType] = group.allMembers
  }

}
