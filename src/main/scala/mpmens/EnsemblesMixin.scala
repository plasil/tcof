package mpmens

import mpmens.model.Component
import org.chocosolver.solver.constraints.nary.cnf.{ILogical, LogOp}
import org.chocosolver.solver.variables.{IntVar, SetVar}

import scala.collection.mutable

trait EnsemblesMixin {
  this: System =>

  class Ensemble[AnchorType <: Component](val name: String, val anchor: AnchorType) extends WithUtility {
    private val roles = mutable.Map.empty[String, Role[_]]
    private[EnsemblesMixin] var membershipClause: Logical = null

    def role[ComponentType <: Component](name: String, items: Members[ComponentType]) = {
      val role = new Role[ComponentType](name, items)
      roles += (name -> role)
      role
    }

    def role[ComponentType <: Component](name: String) = roles(name).asInstanceOf[Role[ComponentType]]

    def membership(clause: Logical): Unit = {
      membershipClause = clause
    }

    override def toString(): String =
      s"""Ensemble "$name" (utility: ${solutionUtility}):
         |  Anchor: ${indent(anchor.toString, 2)}
         |${indent(roles.values.mkString("\n"), 1)}
         |""".stripMargin
  }

  class EnsembleGroup[EnsembleType <: Ensemble[_]](private val generator: () => Array[EnsembleType])
      extends SystemDelegates with WithMembers[EnsembleType] {

    private[mpmens] var allMembers: Members[EnsembleType] = null

    private[mpmens] def setupEnsemblesAndGetMembershipClause() = {
      allMembers = new MembersFromUniverse(generator()) /* TODO - here we should use either MembersFromUniverse or MembersFromParent depending on the nesting */
      setupWithMembers()

      LogicalUtils.conditionMembership(allMembers.map(_.membershipClause), allMembersVar, LogOp.and(_ : _*), true)
    }

    override def toString(): String =
      s"""Ensemble group:
         |${indent(selectedMembers.mkString("\n"), 1)}
         |""".stripMargin
  }

  /** A set of all potential ensembles */
  protected val ensembleGroups = mutable.ListBuffer.empty[EnsembleGroup[_]]

  def ensembles[EnsembleType <: Ensemble[_]](generator: => Array[EnsembleType]): EnsembleGroup[EnsembleType] = {
    val group = new EnsembleGroup(generator _)
    ensembleGroups += group
    group
  }
}
