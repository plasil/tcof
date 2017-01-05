package mpmens

import mpmens.InitStages.InitStages
import org.chocosolver.solver.Model

trait RootEnsembleMixin {
  this: Universe =>

  class RootEnsemble extends Ensemble {
    name("<root>")

    override private[mpmens] def _init(stage: InitStages, config: Config): Unit = {
      super._init(stage, config)

      stage match {
        case InitStages.RulesCreation =>
          for (group <- _ensembleGroups.values) {
            group.allMembers.mapEnsembleActivationRecursive(group)
          }

          val util = if (utility.isEmpty)
            _solverModel.sum(
              _ensembleGroups.values.map(_.sum(_.utility.getOrElse(_solverModel.IntegerInt(0)))) ++
                components.map(_.utility.getOrElse(_solverModel.IntegerInt(0)))
            )
          else
            utility.get

          val sm = _solverModel
          util match {
            case sm.IntegerIntVar(utilityVar) => _solverModel.setObjective(Model.MAXIMIZE, utilityVar)
            case _ =>
          }

          _solverModel.post(_buildEnsembleClause)
        case _ =>
      }
    }
  }

  class RootEnsembleAnchor[EnsembleType <: RootEnsemble] private[mpmens](val builder: () => EnsembleType) {
    private var _solution: EnsembleType = _

    def solution: EnsembleType = _solution

    def init(): Unit = {
      _solution = builder()

      // This is not needed per se because ensembles are discarded in each step anyway. However, component are not. We keep it here for uniformity with components.
      val config = new Config(new SolverModel())
      for (stage <- InitStages.values) {
        _solution._init(stage, config)
      }
    }

    def solve(): Boolean = _solution._solverModel.getSolver.solve()

    def commit(): Unit = {
      solution.executeActions()
      components.foreach(_.executeActions())
    }
  }

  protected def root[EnsembleType <: RootEnsemble](builder: => EnsembleType): RootEnsembleAnchor[EnsembleType] = {
    new RootEnsembleAnchor(builder _)
  }
}
