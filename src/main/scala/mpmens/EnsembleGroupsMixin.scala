package mpmens

import mpmens.model.Component
import org.chocosolver.solver.constraints.nary.cnf.LogOp

import scala.collection.mutable
import scala.reflect.ClassTag

trait EnsembleGroupsMixin {
  this: System =>

  class EnsembleGroupMembers[EnsembleType <: Ensemble[_]](values: Seq[EnsembleType]) extends Members(values) {
    def mapEnsembleActivationRecursive(thisGroup: EnsembleGroup[_], parentGroup: EnsembleGroup[_] = null, ensembleIndexInParent: Int = 0): Unit = {
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
    private[mpmens] val ensembleGroups = mutable.ListBuffer.empty[EnsembleGroup[_]]

    def ensembles[EnsembleType <: Ensemble[_]](ens: Seq[EnsembleType]): EnsembleGroup[EnsembleType] = {
      val group = new EnsembleGroup(new EnsembleGroupMembers(ens))
      ensembleGroups += group
      group
    }

    private[mpmens] def ensembleGroupClause: Logical = LogicalUtils.and(ensembleGroups.map(_.membershipClause))
  }

  class EnsembleGroup[EnsembleType <: Ensemble[_]](private[mpmens] val allMembers: EnsembleGroupMembers[EnsembleType])
    extends SystemDelegates with WithMembers[EnsembleType] {

    private[mpmens] val membershipClause = LogicalUtils.conditionMembership(allMembers.map(_.ensembleClause), allMembersVar, LogOp.and(_ : _*), true)

    override def toString(): String =
      s"""Ensemble group:
         |${indent(selectedMembers.mkString("\n"), 1)}
         |""".stripMargin
  }

}
