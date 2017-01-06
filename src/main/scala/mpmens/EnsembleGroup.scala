package mpmens

import mpmens.InitStages.InitStages
import mpmens.Utils._

class EnsembleGroup[+EnsembleType <: Ensemble](val name: String, private[mpmens] val allMembers: EnsembleGroupMembers[EnsembleType]) extends WithMembers[EnsembleType] with WithConfig with Initializable {

  private[mpmens] var parentGroup: EnsembleGroup[_ <: Ensemble] = null
  private[mpmens] var indexInParentGroup: Int = _

  private[mpmens] def buildMembershipClause = _solverModel.forAllSelected(allMembers.map(_._buildEnsembleClause), allMembersVar)

  override private[mpmens] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)
    allMembers.values.foreach(_._init(stage, config))

    stage match {
      case InitStages.ConfigPropagation =>
        for ((ensemble, idx) <- allMembers.values.zipWithIndex) {
          for (group <- ensemble._ensembleGroups.values) {
            group.parentGroup = this
            group.indexInParentGroup = idx
          }
        }

      case InitStages.RulesCreation =>
        if (parentGroup != null) {
          for (idx <- 0 until allMembers.size) {
            _solverModel.ifThen(_solverModel.member(idx, allMembersVar), _solverModel.member(indexInParentGroup, parentGroup.allMembersVar))
          }
        }

      case _ =>
    }

  }

  override def toString: String =
    s"""Ensemble group "$name":\n${indent(selectedMembers.mkString(""), 1)}"""
}
