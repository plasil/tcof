package mpmens

import org.chocosolver.solver.Model

trait RootEnsembleMixin {
  this: Universe =>

  class RootEnsemble extends Ensemble {
    name("<root>")

    override private[mpmens] def _init(stage: Int): Unit = {
      super._init(stage)

      stage match {
        case 1 =>
          for (group <- _ensembleGroups.values) {
            group.allMembers.mapEnsembleActivationRecursive(group)
          }

          val util = if (utility.isEmpty)
            IntegerUtils.sum(
              _ensembleGroups.values.map(_.sum(_.utility.getOrElse(IntegerInt(0)))) ++
                components.map(_.utility.getOrElse(IntegerInt(0)))
            )
          else
            utility.get

          util match {
            case IntegerIntVar(utilityVar) => solverModel.setObjective(Model.MAXIMIZE, utilityVar)
            case _ =>
          }

          LogicalUtils.post(_buildEnsembleClause)
        case _ =>
      }
    }
  }

  class RootEnsembleAnchor[EnsembleType <: RootEnsemble] private[mpmens](val builder: () => EnsembleType) {
    private var _solution: EnsembleType = _

    private var solverModel: Model = _

    def solution: EnsembleType = _solution

    def init(): Unit = {
      _solution = builder()

      // This is not needed per se because ensembles are discarded in each step anyway. However, component are not. We keep it here for uniformity with components.
      solverModel = new Model()
      for (stage <- 0 until InitStages) {
        _solution._init(stage)
      }
    }

    def solve(): Unit = {
      solverModel.getSolver.solve()
    }

    def commit(): Unit = {
      rootEnsemble.executeActions()
      components.foreach(_.executeActions())
    }
  }
}
