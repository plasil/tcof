package mpmens

import mpmens.Utils._

class StateSet[+StateType <: State](val name: String, private[mpmens] val allMembers: StateSetMembers[StateType]) extends WithMembers[StateType] with Initializable {

  override def toString: String =
    s"""State set "$name":\n${indent(selectedMembers.mkString(""), 1)}"""
}
