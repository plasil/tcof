package mpmens

import mpmens.model.Component
import org.chocosolver.solver.Model
import org.chocosolver.solver.constraints.nary.cnf.{ILogical, LogOp}
import org.chocosolver.solver.variables.{BoolVar, IntVar, SetVar}

import scala.collection.mutable

class System extends WithSolverModel with MembersMixin with ImplicitsMixin with RolesMixin with EnsemblesMixin {
  system =>

  /** Model used by the solver. */
  private[mpmens] val solverModel = new Model()

  /** Solver variable representing the total utility of the system */
  private var totalUtilityVar: IntVar = null

  /** Internal method used in pretty-printing solving results */
  private[mpmens] def indent(str: String, level: Int) = str.lines.map("  " * level + _).mkString("\n")

  trait WithSolverModelFromSystem extends WithSolverModel {
    def solverModel = system.solverModel
  }

  /** A set of all potential ensembles */
  private val ensembleGroups = mutable.ListBuffer.empty[EnsembleGroup[_]]

  def ensembles[EnsembleType <: Ensemble[_]](generator: => Array[EnsembleType]): EnsembleGroup[EnsembleType] = {
    val group = new EnsembleGroup(generator _)
    ensembleGroups += group
    group
  }

  def restart(): Unit = {
    val utilityVars = for (x <- potentialEnsembles if x.utilityVar != null) yield x.utilityVar
    if (utilityVars.size > 0) {
      totalUtilityVar = solverModel.intVar(IntMinValue, IntMaxValue)
      solverModel.sum(utilityVars toArray, "=", totalUtilityVar).post()
      solverModel.setObjective(Model.MAXIMIZE, totalUtilityVar)
    }
  }

  def solve(): Boolean = {
    solverModel.getSolver().solve()
  }

  def solutionUtility: Int =
    if (totalUtilityVar != null && totalUtilityVar.isInstantiated)
      totalUtilityVar.getValue
    else
      0

  override def toString(): String =
    s"""System (total utility: ${totalUtility}):
       |${ensembles.mkString("\n")}
     """.stripMargin
}
