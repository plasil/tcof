package mpmens

import example.Incident
import mpmens.model.Component
import org.chocosolver.solver.Model

import scala.collection.mutable


class System extends SolverMixin with LogicalMixin with IntegerMixin with WithMembersUtilsMixin with RoleMembersMixin with ImplicitsMixin with RolesMixin with EnsembleGroupsMixin with EnsemblesMixin {
  systemThis =>

  trait SystemDelegates extends WithSystemDelegates {
    def system = systemThis
    def solverModel = systemThis.solverModel
  }

  /** Internal method used in pretty-printing solving results */
  private[mpmens] def indent(str: String, level: Int) = str.lines.map("  " * level + _).mkString("\n")

//  private val rootEnsembleGroup = new EnsembleGroup(new EnsembleGroupMembers(Array(new RootEnsemble)))

  def ensembles[EnsembleType <: Ensemble[_]](ens: Seq[EnsembleType]): EnsembleGroup[EnsembleType] = rootEnsemble.ensembles(ens)
  def utility(cst: Integer) = rootEnsemble.utility(cst)
  def membership(clause: Logical): Unit = rootEnsemble.membership(clause)

  private var rootEnsembleInit: () => Unit = null

  def system(initFun: => Unit): Unit = {
    rootEnsembleInit = initFun _
  }

  class RootEnsemble extends Ensemble[Null]("root", null) {
    // Though ugly, this is needed the delegators above (ensembles, utility) call this instance from the rootEnsembleInit below.
    rootEnsemble = this

    rootEnsembleInit()

    for (group <- ensembleGroups) {
      group.allMembers.mapEnsembleActivationRecursive(group)
    }

    utility match {
      case IntegerIntVar(utilityVar) => solverModel.setObjective(Model.MAXIMIZE, utilityVar)
      case _ =>
    }

    LogicalUtils.post(ensembleClause)
  }

  private var rootEnsemble: RootEnsemble = null

  private var _universe: Seq[Component] = Seq()

  def universe(univ: Seq[Component]): Unit = {
    _universe = univ
  }

  def universe: Seq[Component] = _universe

  def init(): Unit = {
    new RootEnsemble()
  }

  def solve(): Boolean = {
    solverModel.getSolver().solve()
  }

  def solutionUtility: Int = rootEnsemble.solutionUtility

  override def toString(): String = rootEnsemble.toString
}
