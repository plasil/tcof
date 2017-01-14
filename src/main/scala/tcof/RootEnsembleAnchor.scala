package tcof

class RootEnsembleAnchor[EnsembleType <: RootEnsemble] private[tcof](val builder: () => EnsembleType) {
  private var _solution: EnsembleType = _

  def instance: EnsembleType = _solution

  def init(): Unit = {
    _solution = builder()

    // This is not needed per se because ensembles are discarded in each step anyway. However, component are not. We keep it here for uniformity with components.
    val config = new Config(new SolverModel())
    for (stage <- InitStages.values) {
      _solution._init(stage, config)
    }

    instance._executePreActions()
  }

  def solve(): Boolean = _solution._solverModel.solveAndRecord()

  def commit(): Unit = {
    instance._executeActions()
  }
}

