package mpmens

import org.chocosolver.solver.Model


abstract class Universe extends LogicalMixin with IntegerMixin with WithMembersUtilsMixin with ImplicitsMixin with RolesMixin with EnsembleGroupsMixin with StateSetsMixin with EnsemblesMixin with ComponentsMixin with RootEnsembleMixin with ActionsMixin {
  uniThis =>

  trait SystemDelegates extends WithSystemDelegates {
    val universe: Universe = uniThis
    def solverModel: Model = uniThis.solverModel
  }

  /** Model used by the solver. */
  private[mpmens] var solverModel: Model = null

  /** Upper bound for integer variables of the solver */
  private[mpmens] val IntMaxValue = 10000 // IntVar.MAX_INT_BOUND
  /** Lower bound for integer variables of the solver */
  private[mpmens] val IntMinValue = -10000 // IntVar.MIN_INT_BOUND

  private[mpmens] def newIntVar = solverModel.intVar(IntMinValue, IntMaxValue)

  private[mpmens] val InitStages = 2


  private var _universe = Seq.empty[Component]
  def components_= (univ: Seq[Component]): Unit = _universe = univ
  def components: Seq[Component] = _universe

  def init(): Unit = {
    rootEnsemble = root

    // This is not needed per se because ensembles are discarded in each step anyway. However, component are not. We keep it here for uniformity with components.
    solverModel = new Model()
    for (stage <- 0 until InitStages) {
      rootEnsemble._init(stage)
      components.foreach(_._init(stage))
    }

    println(solverModel)
  }

  def solve(): Boolean = {
    solverModel.getSolver.solve()
  }

  def commit(): Unit = {
    rootEnsemble.executeActions()
    components.foreach(_.executeActions())
  }

  def solutionUtility: Int = rootEnsemble.solutionUtility

  override def toString: String = rootEnsemble.toString + "\n" + components.mkString("\n")

  def root: RootEnsemble
}
