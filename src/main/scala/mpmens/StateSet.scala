package mpmens

import mpmens.InitStages.InitStages
import mpmens.Utils._

class StateSet[+StateType <: State](stateId: Int, name: String, private[mpmens] val allMembers: StateSetMembers[StateType]) extends State(stateId, name) with WithMembers[StateType] with Initializable {

  for ((subState, idx) <- allMembers.values.zipWithIndex) {
    require(subState.parent == null)

    subState.parent = this
    subState.indexInParent = idx
  }

  override private[mpmens] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.RulesCreation =>
        if (parent != null) {
          for (idx <- 0 until allMembers.size) {
            _solverModel.ifThen(_solverModel.member(idx, allMembersVar), _solverModel.member(indexInParent, parent.allMembersVar))
          }
        }

      case _ =>
    }
  }


  override def toString: String =
    s"""\nState set "$name":${indent(selectedMembers.mkString(""), 1)}"""
}
