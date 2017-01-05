package mpmens

import mpmens.Utils._

import scala.collection.mutable

trait StateSetsMixin {
  this: Universe =>

  class State

  class StateSetMembers[+StateType <: State](values: Iterable[StateType]) extends Members(values)

  trait WithStateSets extends Initializable {
    /** A set of all potential ensembles */
    private[mpmens] val _stateSets = mutable.Map.empty[String, StateSet[State]]

    def states[StateType <: State](stateFirst: StateType, stateRest: StateType*): StateSet[StateType] = states(randomName, stateRest.+:(stateFirst))
    def states[StateType <: State](stats: Iterable[StateType]): StateSet[StateType] = states(randomName, stats)
    def states[StateType <: State](name: String, stateFirst: StateType, stateRest: StateType*): StateSet[StateType] = states(name, stateRest.+:(stateFirst))

    def states[StateType <: State](name: String, stats: Iterable[StateType]): StateSet[StateType] = {
      val stateSet = new StateSet(name, new StateSetMembers(stats))
      _stateSets += name -> stateSet
      stateSet
    }

    /* TODO: This needs to be changed to be re-initializable components
    def statesExclusive[StateType <: State](stateFirst: StateType, stateRest: StateType*): StateSet[StateType] = statesExclusive(randomName, stateRest.+:(stateFirst))
    def statesExclusive[StateType <: State](stats: Iterable[StateType]): StateSet[StateType] = statesExclusive(randomName, stats)
    def statesExclusive[StateType <: State](name: String, stateFirst: StateType, stateRest: StateType*): StateSet[StateType] = statesExclusive(name, stateRest.+:(stateFirst))

    def statesExclusive[StateType <: State](name: String, stats: Iterable[StateType]): StateSet[StateType] = {
      val stateSet = states(name, stats)
      solverModel.arithm(stateSet.allMembersVar.getCard, "=", 0).post()
      stateSet
    }
    */

    def states[StateType <: State](name: String): StateSet[StateType] = _stateSets(name).asInstanceOf[StateSet[StateType]]

    override private[mpmens] def _init(stage: Int) = {
      super._init(stage)
      _stateSets.values.foreach(_._init(stage))
    }
  }

  class StateSet[+StateType <: State](val name: String, private[mpmens] val allMembers: StateSetMembers[StateType])
    extends SystemDelegates with WithMembers[StateType] with Initializable {

    override def toString: String =
      s"""State set "$name":\n${indent(selectedMembers.mkString(""), 1)}"""
  }

}
