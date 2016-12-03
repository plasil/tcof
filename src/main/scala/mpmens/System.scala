package mpmens

import mpmens.model.Component
import org.chocosolver.solver.Model
import org.chocosolver.solver.constraints.nary.cnf.{ILogical, LogOp}
import org.chocosolver.solver.variables.{BoolVar, IntVar, SetVar}

import scala.collection.mutable

class System extends WithSolverModel with LogicalMixin with IntegerMixin with MembersMixin with ImplicitsMixin with RolesMixin with EnsemblesMixin with WithUtility {
  system =>



  trait SystemDelegates extends WithSolverModel with LogicalHelper with IntegerHelper {
    def solverModel = system.solverModel

    def sumBasedOnMembership(membersVar: SetVar, values: Array[Integer]): IntegerIntVar = sumBasedOnMembership(membersVar, values)
  }

  /** Model used by the solver. */
  private[mpmens] val solverModel = new Model()

  /** Solver variable representing the total utility of the system */
  private var totalUtilityVar: IntVar = null

  /** Internal method used in pretty-printing solving results */
  private[mpmens] def indent(str: String, level: Int) = str.lines.map("  " * level + _).mkString("\n")

  /** A set of all potential ensembles */
  private val ensembleGroups = mutable.ListBuffer.empty[EnsembleGroup[_]]

  def ensembles[EnsembleType <: Ensemble[_]](generator: => Array[EnsembleType]): EnsembleGroup[EnsembleType] = {
    val group = new EnsembleGroup(generator _)
    ensembleGroups += group
    group
  }

  def restart(): Unit = {
    utility match {
      case IntegerIntVar(utilityVar) => solverModel.setObjective(Model.MAXIMIZE, utilityVar)
    }

    val clauses = ensembleGroups.map(_.setupEnsemblesAndGetMembershipClause())

    clauses.
  }

  def solve(): Boolean = {
    solverModel.getSolver().solve()
  }

  override def toString(): String =
    s"""System (total utility: ${solutionUtility}):
       |${ensembles.mkString("\n")}
     """.stripMargin
}
