package mpmens

import mpmens.InitStages.InitStages
import mpmens.Utils._

import scala.collection.mutable

trait EnsembleGroupsMixin {
  this: Universe =>

  class EnsembleGroupMembers[+EnsembleType <: Ensemble](values: Iterable[EnsembleType]) extends Members(values) with WithConfig {
    def mapEnsembleActivationRecursive(thisGroup: EnsembleGroup[Ensemble], parentGroup: EnsembleGroup[Ensemble] = null, ensembleIndexInParent: Int = 0): Unit = {
      if (parentGroup != null) {
        for (idx <- 0 until size) {
          _solverModel.ifThen(_solverModel.member(idx, thisGroup.allMembersVar), _solverModel.member(ensembleIndexInParent, parentGroup.allMembersVar))
        }
      }

      var idx = 0
      for (ensemble <- values) {
        for (group <- ensemble._ensembleGroups.values) {
          group.allMembers.mapEnsembleActivationRecursive(group, thisGroup, idx)
        }

        idx = idx + 1
      }
    }
  }

  class EnsembleGroup[+EnsembleType <: Ensemble](val name: String, private[mpmens] val allMembers: EnsembleGroupMembers[EnsembleType])
    extends WithMembers[EnsembleType] with WithConfig with Initializable {

    private[mpmens] def buildMembershipClause = _solverModel.forAllSelected(allMembers.map(_._buildEnsembleClause), allMembersVar)

    override private[mpmens] def _init(stage: InitStages, config: Config): Unit = {
      super._init(stage, config)
      allMembers._init(stage, config)
      allMembers.values.foreach(_._init(stage, config))
    }

    override def toString: String =
      s"""Ensemble group "$name":\n${indent(selectedMembers.mkString(""), 1)}"""
  }

  trait WithEnsembleGroups extends Initializable {
    this: WithConfig =>

    /** A set of all potential ensembles */
    private[mpmens] val _ensembleGroups = mutable.Map.empty[String, EnsembleGroup[Ensemble]]

    def ensembles[EnsembleType <: Ensemble](ensFirst: EnsembleType, ensRest: EnsembleType*): EnsembleGroup[EnsembleType] = ensembles(randomName, ensRest.+:(ensFirst))

    def ensembles[EnsembleType <: Ensemble](ens: Iterable[EnsembleType]): EnsembleGroup[EnsembleType] = ensembles(randomName, ens)

    def ensembles[EnsembleType <: Ensemble](name: String, ensFirst: EnsembleType, ensRest: EnsembleType*): EnsembleGroup[EnsembleType] = ensembles(name, ensRest.+:(ensFirst))

    def ensembles[EnsembleType <: Ensemble](name: String, ens: Iterable[EnsembleType]): EnsembleGroup[EnsembleType] = {
      val group = new EnsembleGroup(name, new EnsembleGroupMembers(ens))
      _ensembleGroups += name -> group
      group
    }

    private[mpmens] def _buildEnsembleGroupClause: Logical = _solverModel.and(_ensembleGroups.values.map(_.buildMembershipClause))

    override private[mpmens] def _init(stage: InitStages, config: Config): Unit = {
      super._init(stage, config)
      _ensembleGroups.values.foreach(_._init(stage, config))
    }
  }

}
