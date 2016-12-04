package mpmens

import org.chocosolver.solver.constraints.nary.cnf.LogOp

import scala.collection.mutable
import scala.reflect.ClassTag

trait EnsembleGroupsMixin {
  this: Universe =>

  class EnsembleGroupMembers[+EnsembleType <: Ensemble](values: Seq[EnsembleType]) extends Members(values) {
    def mapEnsembleActivationRecursive(thisGroup: EnsembleGroup[Ensemble], parentGroup: EnsembleGroup[Ensemble] = null, ensembleIndexInParent: Int = 0): Unit = {
      if (parentGroup != null) {
        for (idx <- 0 until size) {
          solverModel.ifThen(solverModel.member(idx, thisGroup.allMembersVar), solverModel.member(ensembleIndexInParent, parentGroup.allMembersVar))
        }
      }

      for (idx <- 0 until size) {
        val ensemble = values(idx)

        for (group <- ensemble.ensembleGroups) {
          group.allMembers.mapEnsembleActivationRecursive(group, thisGroup, idx)
        }
      }
    }
  }

  trait WithEnsembleGroups {
    /** A set of all potential ensembles */
    private[mpmens] val ensembleGroups = mutable.ListBuffer.empty[EnsembleGroup[Ensemble]]

    def ensembles[EnsembleType <: Ensemble](ensFirst: EnsembleType, ensRest: EnsembleType*): EnsembleGroup[EnsembleType] = ensembles(ensRest.+:(ensFirst))

    def ensembles[EnsembleType <: Ensemble](ens: Seq[EnsembleType]): EnsembleGroup[EnsembleType] = {
      val group = new EnsembleGroup(new EnsembleGroupMembers(ens))
      ensembleGroups += group
      group
    }

    private[mpmens] def ensembleGroupClause: Logical = LogicalUtils.and(ensembleGroups.map(_.membershipClause))
  }

  class EnsembleGroup[+EnsembleType <: Ensemble](private[mpmens] val allMembers: EnsembleGroupMembers[EnsembleType])
    extends SystemDelegates with WithMembers[EnsembleType] {

    private[mpmens] val membershipClause = LogicalUtils.forAllSelected(allMembers.map(_.ensembleClause), allMembersVar)

    override def toString(): String =
      s"""Ensemble group:\n${indent(selectedMembers.mkString(""), 1)}"""
  }

}
