package rcrs.scenario

import mpmens.traits.map2d.Position
import mpmens.{Component, Universe}

trait MobileUnitComponent {
  this: Universe with ObservationSupport with RegistrationSupport with AreaExplorationSupport =>

  abstract class MobileUnit(var position: Position) extends Component with PositionAware with Registration with AreaExploration with Observation {

    val Stopped = State

    val Operation = StateOr(Register, AreaExploration, Stopped)

    constraints(
      Operation
    )

    utility(
      states.sum {
        case Observation => 1
        case AreaExploration => 1
      }
    )

  }

}
