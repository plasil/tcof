package mpmens

import org.chocosolver.solver.Model

import scala.collection.mutable


class System extends SolverMixin with LogicalMixin with IntegerMixin with MembersMixin with ImplicitsMixin with RolesMixin with EnsemblesMixin with WithUtility {
  systemThis =>

  trait SystemDelegates extends WithSystemDelegates {
    def system = systemThis
    def solverModel = systemThis.solverModel
  }

  /** Internal method used in pretty-printing solving results */
  private[mpmens] def indent(str: String, level: Int) = str.lines.map("  " * level + _).mkString("\n")

  def restart(): Unit = {
    utility match {
      case IntegerIntVar(utilityVar) => solverModel.setObjective(Model.MAXIMIZE, utilityVar)
    }

    val clauses = ensembleGroups.map(_.setupEnsemblesAndGetMembershipClause())
    val systemClause = LogicalUtils.and(clauses)

    LogicalUtils.post(systemClause)
  }

  def solve(): Boolean = {
    solverModel.getSolver().solve()
  }

  override def toString(): String =
    s"""System (total utility: ${solutionUtility}):
       |${ensembleGroups.mkString("\n")}
     """.stripMargin
}
