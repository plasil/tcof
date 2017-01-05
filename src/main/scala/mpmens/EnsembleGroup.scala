package mpmens

import mpmens.InitStages.InitStages
import mpmens.Utils._

class EnsembleGroup[+EnsembleType <: Ensemble](val name: String, private[mpmens] val allMembers: EnsembleGroupMembers[EnsembleType]) extends WithMembers[EnsembleType] with WithConfig with Initializable {

  private[mpmens] def buildMembershipClause = _solverModel.forAllSelected(allMembers.map(_._buildEnsembleClause), allMembersVar)

  override private[mpmens] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)
    allMembers._init(stage, config)
    allMembers.values.foreach(_._init(stage, config))
  }

  override def toString: String =
    s"""Ensemble group "$name":\n${indent(selectedMembers.mkString(""), 1)}"""
}
