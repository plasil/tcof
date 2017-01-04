package rcrs

import mpmens.Universe
import mpmens.traits.map2d.{Map2DTrait, Position, PositionAware}
import rcrs.traits.RCRSConnectorTrait
import rcrs.traits.map2d.RCRSNodeStatus
import rcrs.traits.time.CurrentTimeTrait

trait XXX {
  this: RescueScenario#PoliceForce =>

  position = Position(3,4)
}

class RescueScenario extends Universe with RCRSConnectorTrait with Map2DTrait[RCRSNodeStatus] with CurrentTimeTrait {

  trait Explorer {
    var explorationZone: MapZone = null
  }

  abstract class MobileUnit(name: String, var position: Position) extends Component(name) with PositionAware with Explorer

  class PoliceForce(_position: Position) extends MobileUnit("PoliceForce", _position) with XXX
  class FireBrigade(_position: Position) extends MobileUnit("FireBrigade", _position)

  class AmbulanceTeam(_position: Position) extends MobileUnit("AmbulanceTeam", _position) {

    val mode = modes(Exploration, ColisionAvoidance)

    constraints(
      Exploration -> (explorationZone != null) &&
        allExclusive(Exploration, Rest)
    )

    utility =
      Exploration -> 5 +
        Rest -> 3

    actions {
      modes.selectedMembers.foreach {
        case Exploration => exploration
        case Rest => rest
      }
    }
  }

  class ExplorationTeams(val zone: MapZone) extends Ensemble(s"ExplorationTeam for $zone") {
    val mobileUnits = role("mobileUnits", components.select[MobileUnit])

    val fireBrigades = role("fireBrigades", mobileUnits.selectEquiv[FireBrigade])
    val ambulances = role("ambulanceTeams", mobileUnits.selectEquiv[AmbulanceTeam])
    val police = role("policeForces", mobileUnits.selectEquiv[PoliceForce])

    membership(
      fireBrigades.cardinality >= 1 &&
        ambulances.cardinality >= 1 &&
        police.cardinality >= 1
    )

    def proximityToZoneCenter(unit: MobileUnit) = 100 - (unit.position.distanceTo(zone.center) / 10000).round.toInt

    utility = mobileUnits.sum(proximityToZoneCenter(_))

    actions {
      mobileUnits.foreachBySelection(_.explorationZone = zone, _.explorationZone = null)
    }
  }

  system {
    val mapZones = for {
      xIdx <- 0 until 2
      yIdx <- 0 until 2
    } yield new MapZone(map, xIdx, yIdx, time - 20)

    val explorationTeams = ensembles("explorationTeams", mapZones.map(new ExplorationTeams(_)))

    membership(
      explorationTeams.map(_.fireBrigades).allDisjoint &&
        explorationTeams.map(_.ambulances).allDisjoint &&
        explorationTeams.map(_.police).allDisjoint
    )
  }

  ambulance1.mode.setSolution(.....)
}
