package mpmens

import mpmens.InitStages.InitStages

class StateSetExclusive[+StateType <: State](name: String, allMembers: StateSetMembers[StateType]) extends StateSet(name, allMembers) {
  override private[mpmens] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.RulesCreation =>
        _solverModel.arithm(allMembersVar.getCard, "=", 1).post()
      case _ =>
    }
  }

}
