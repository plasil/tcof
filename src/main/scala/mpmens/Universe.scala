package mpmens

import example.Incident
import org.chocosolver.solver.Model

import scala.collection.mutable


class Universe extends LogicalMixin with IntegerMixin with WithMembersUtilsMixin with RoleMembersMixin with ImplicitsMixin with RolesMixin with EnsembleGroupsMixin with EnsemblesMixin {
  uniThis =>

  trait SystemDelegates extends WithSystemDelegates {
    val universe = uniThis
    def solverModel = uniThis.solverModel
  }

  /** Internal method used in pretty-printing solving results */
  private[mpmens] def indent(str: String, level: Int) = str.lines.map("  " * level + _).mkString("\n") + (if (str.endsWith("\n")) "\n" else "")

  def ensembles[EnsembleType <: Ensemble](ens: Seq[EnsembleType]): EnsembleGroup[EnsembleType] = rootEnsemble.ensembles(ens)
  def ensembles[EnsembleType <: Ensemble](ensFirst: EnsembleType, ensRest: EnsembleType*): EnsembleGroup[EnsembleType] = ensembles(ensRest.+:(ensFirst))
  def utility_= (cst: Integer): Unit = rootEnsemble.utility = cst
  def utility: Integer = rootEnsemble.utility
  def membership(clause: Logical): Unit = rootEnsemble.membership(clause)

  private var rootEnsembleInit: () => Unit = null

  def systems(initFun: => Unit): Unit = {
    rootEnsembleInit = initFun _
  }

  /** Upper bound for integer variables of the solver */
  private[mpmens] val IntMaxValue = 10000 // IntVar.MAX_INT_BOUND
  /** Lower bound for integer variables of the solver */
  private[mpmens] val IntMinValue = -10000 // IntVar.MIN_INT_BOUND

  private[mpmens] def newIntVar = solverModel.intVar(IntMinValue, IntMaxValue)

  /** Model used by the solver. */
  private[mpmens] def solverModel = rootEnsemble.solverModel

  class RootEnsemble extends Ensemble("<root>") {
    // Though ugly, this is needed the delegators above (ensembles, utility) call this instance from the rootEnsembleInit below.
    rootEnsemble = this
    private[mpmens] val solverModel = new Model()

    rootEnsembleInit()

    for (group <- ensembleGroups) {
      group.allMembers.mapEnsembleActivationRecursive(group)
    }

    if (utility == null) {
      utility = IntegerUtils.sum(ensembleGroups.map(_.sum(_.utility)))
    }

    utility match {
      case IntegerIntVar(utilityVar) => solverModel.setObjective(Model.MAXIMIZE, utilityVar)
      case _ =>
    }

    LogicalUtils.post(ensembleClause)
  }

  private var rootEnsemble: RootEnsemble = null

  private var _universe: Seq[Component] = Seq()

  def components_= (univ: Seq[Component]): Unit = _universe = univ

  def components: Seq[Component] = _universe

  def init(): Unit = {
    new RootEnsemble()
  }

  def solve(): Boolean = {
    solverModel.getSolver().solve()
  }

  def solutionUtility: Int = rootEnsemble.solutionUtility

  override def toString(): String = rootEnsemble.toString
}
