package mpmens

import mpmens.Utils._

import scala.collection.mutable

trait EnsembleGroupsMixin {
  this: Universe =>

  class EnsembleGroupMembers[+EnsembleType <: Ensemble](values: Iterable[EnsembleType]) extends Members(values) {
    def mapEnsembleActivationRecursive(thisGroup: EnsembleGroup[Ensemble], parentGroup: EnsembleGroup[Ensemble] = null, ensembleIndexInParent: Int = 0): Unit = {
      if (parentGroup != null) {
        for (idx <- 0 until size) {
          solverModel.ifThen(solverModel.member(idx, thisGroup.allMembersVar), solverModel.member(ensembleIndexInParent, parentGroup.allMembersVar))
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

  trait WithEnsembleGroups extends Initializable {
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

    def ensembles[EnsembleType <: Ensemble](name: String): EnsembleGroup[EnsembleType] = _ensembleGroups(name).asInstanceOf[EnsembleGroup[EnsembleType]]

    private[mpmens] def _buildEnsembleGroupClause: Logical = LogicalUtils.and(_ensembleGroups.values.map(_.buildMembershipClause))

    override private[mpmens] def _init(stage: Int) = {
      super._init(stage)
      _ensembleGroups.values.foreach(_._init(stage))
    }
  }

  class EnsembleGroup[+EnsembleType <: Ensemble](val name: String, private[mpmens] val allMembers: EnsembleGroupMembers[EnsembleType])
    extends SystemDelegates with WithMembers[EnsembleType] with Initializable {

    private[mpmens] def buildMembershipClause = LogicalUtils.forAllSelected(allMembers.map(_._buildEnsembleClause), allMembersVar)

    override private[mpmens] def _init(stage: Int) = {
      super._init(stage)
      allMembers.values.foreach(_._init(stage))
    }

    override def toString: String =
      s"""Ensemble group "$name":\n${indent(selectedMembers.mkString(""), 1)}"""
  }

}
