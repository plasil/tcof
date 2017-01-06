package rcrs.scenario

import mpmens.traits.map2d.{Position, PositionAware}
import mpmens.{Component, Universe}

trait MobileUnitComponent {
  this: Universe with ObservationSupport with RegistrationSupport with AreaExplorationSupport =>

  abstract class MobileUnit(var position: Position) extends Component with PositionAware with Registration with AreaExploration with Observation {

    val Operation = StateOr(Register, AreaExploration)

    utility(
      states.sum {
        case Register => 2
        case AreaExploration => 2
      }
    )

  }

}
