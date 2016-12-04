package mpmens

import org.chocosolver.solver.constraints.nary.cnf.LogOp

import scala.collection.mutable
import scala.reflect.ClassTag

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
        for (group <- ensemble.ensembleGroups.values) {
          group.allMembers.mapEnsembleActivationRecursive(group, thisGroup, idx)
        }

        idx = idx + 1
      }
    }
  }

  trait WithEnsembleGroups {
    /** A set of all potential ensembles */
    private[mpmens] val ensembleGroups = mutable.Map.empty[String, EnsembleGroup[Ensemble]]

    def ensembles[EnsembleType <: Ensemble](ensFirst: EnsembleType, ensRest: EnsembleType*): EnsembleGroup[EnsembleType] = ensembles(randomName, ensRest.+:(ensFirst))

    def ensembles[EnsembleType <: Ensemble](ens: Iterable[EnsembleType]): EnsembleGroup[EnsembleType] = ensembles(randomName, ens)

    def ensembles[EnsembleType <: Ensemble](name: String, ensFirst: EnsembleType, ensRest: EnsembleType*): EnsembleGroup[EnsembleType] = ensembles(name, ensRest.+:(ensFirst))

    def ensembles[EnsembleType <: Ensemble](name: String, ens: Iterable[EnsembleType]): EnsembleGroup[EnsembleType] = {
      val group = new EnsembleGroup(name, new EnsembleGroupMembers(ens))
      ensembleGroups += name -> group
      group
    }

    def ensembles[EnsembleType <: Ensemble](name: String) = ensembleGroups(name).asInstanceOf[EnsembleGroup[EnsembleType]]

    private[mpmens] def ensembleGroupClause: Logical = LogicalUtils.and(ensembleGroups.values.map(_.membershipClause))
  }

  class EnsembleGroup[+EnsembleType <: Ensemble](val name: String, private[mpmens] val allMembers: EnsembleGroupMembers[EnsembleType])
    extends SystemDelegates with WithMembers[EnsembleType] {

    private[mpmens] val membershipClause = LogicalUtils.forAllSelected(allMembers.map(_.ensembleClause), allMembersVar)

    override def toString(): String =
      s"""Ensemble group "$name":\n${indent(selectedMembers.mkString(""), 1)}"""
  }

}
