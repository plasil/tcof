package rcrs.scenario

import tcof._
import tcof.traits.map2d.{Map2DTrait, Position}
import rcrs.traits.RCRSConnectorTrait
import rcrs.traits.map2d.RCRSNodeStatus


class RescueScenario extends Universe with RCRSConnectorTrait with Map2DTrait[RCRSNodeStatus]
  with MobileUnitComponent with RegistrationSupport with AreaExplorationSupport with ObservationSupport {

  class PoliceForce(no: Int, _position: Position) extends MobileUnit(_position) {
    name(s"PoliceForce $no")
  }

  class AmbulanceTeam(no: Int, _position: Position) extends MobileUnit(_position) {
    name(s"AmbulanceTeam $no")
  }

  class FireBrigade(no: Int, _position: Position) extends MobileUnit(_position) {
    name(s"FireBrigade $no")
  }

/*
  class ExplorationTeams(val zone: MapZone) extends Ensemble {
    name(s"ExplorationTeam for $zone")

    val mobileUnits = role("mobileUnits", components.select[MobileUnit])

    // implicit conversion, shortens e.g. mobileUnits.cloneEquiv.selectEquiv[FireBrigade]
    // to mobileUnits.selectEquiv[FireBrigade]
    implicit def roleToRoleMembersEquiv(role: Role[RescueScenario.this.MobileUnit]) = role.cloneEquiv

    val fireBrigades = role("fireBrigades", mobileUnits.selectEquiv[FireBrigade])
    val ambulances = role("ambulanceTeams", mobileUnits.selectEquiv[AmbulanceTeam])
    val police = role("policeForces", mobileUnits.selectEquiv[PoliceForce])

    membership(
      fireBrigades.cardinality >= 1 &&
      ambulances.cardinality >= 1 &&
        police.cardinality === 1
    )

    def proximityToZoneCenter(unit: MobileUnit) = 100 - (unit.position.distanceTo(zone.center) / 10000).round.toInt

    utility(
      mobileUnits.sum(proximityToZoneCenter(_))
    )

    actions {
      mobileUnits.foreachBySelection(_.explorationZone = zone, _.explorationZone = null)
    }
  }

  class System extends RootEnsemble {
    val mapZones = for {
      xIdx <- 0 until 1
      yIdx <- 0 until 2
    } yield new MapZone(map, xIdx, yIdx, 0 /* time - 20 */)

    val explorationTeams = ensembles("explorationTeams", mapZones.map(new ExplorationTeams(_)))

    membership(
      explorationTeams.map(_.fireBrigades).allDisjoint &&
      explorationTeams.map(_.ambulances).allDisjoint &&
      explorationTeams.map(_.police).allDisjoint
    )
  }

  val rootEnsemble = root(new System)
*/

}

