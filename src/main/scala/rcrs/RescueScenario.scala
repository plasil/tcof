package rcrs

import mpmens.Universe
import mpmens.traits.map2d.{Map2DTrait, Position, PositionAware}
import rcrs.traits.map2d.RCRSNodeStatus

class RescueScenario extends Universe with /* RCRSConnectorTrait with */ Map2DTrait[RCRSNodeStatus] /* with CurrentTimeTrait */ {

  trait Explorer {
    var explorationZone: MapZone = null
  }

  abstract class MobileUnit(var position: Position) extends Component with PositionAware with Explorer

  class PoliceForce(no: Int, _position: Position) extends MobileUnit(_position) {
    name(s"PoliceForce $no")
  }

  class AmbulanceTeam(no: Int, _position: Position) extends MobileUnit(_position) {
    name(s"AmbulanceTeam $no")
  }

  class FireBrigade(no: Int, _position: Position) extends MobileUnit(_position) {
    name(s"FireBrigade $no")

    object Exploration extends State
    object ColisionAvoidance extends State

    val oper = states(Exploration, ColisionAvoidance)

    constraints(
      oper.contains(Exploration) -> (explorationZone != null)
    )

    utility(
      oper.sum {
        case Exploration => 5
        case ColisionAvoidance => 3
      }
    )

    actions {
      oper.selectedMembers.foreach {
        case Exploration => exploration()
        case ColisionAvoidance => randomWalk()
      }
    }

    def exploration() {}
    def randomWalk() {}
  }

  class ExplorationTeams(val zone: MapZone) extends Ensemble {
    name(s"ExplorationTeam for $zone")

    val mobileUnits = role("mobileUnits", components.select[MobileUnit])

    val fireBrigades = role("fireBrigades", mobileUnits.selectEquiv[FireBrigade])
//    val ambulances = role("ambulanceTeams", mobileUnits.selectEquiv[AmbulanceTeam])
//    val police = role("policeForces", mobileUnits.selectEquiv[PoliceForce])

    membership(
      fireBrigades.cardinality >= 1
//      ambulances.cardinality >= 1 &&
//      police.cardinality >= 1
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
      xIdx <- 0 until 2
      yIdx <- 0 until 2
    } yield new MapZone(map, xIdx, yIdx, 0 /* time - 20 */)

    val explorationTeams = ensembles("explorationTeams", mapZones.map(new ExplorationTeams(_)))

    membership(
      explorationTeams.map(_.fireBrigades).allDisjoint
//        explorationTeams.map(_.ambulances).allDisjoint &&
//        explorationTeams.map(_.police).allDisjoint
    )
  }

  val rootEnsemble = root(new System)
}

object Test {
  def main(args: Array[String]): Unit = {
    val scenario = new RescueScenario

    scenario.components = List(
      new scenario.FireBrigade(1, Position(391738, 3370)),
      new scenario.FireBrigade(2, Position(424810, 354780))
/*
      new scenario.FireBrigade(Position(48738, 145870)),
      new scenario.FireBrigade(Position(187810, 248325)),
      new scenario.AmbulanceTeam(Position(128728, 82480)),
      new scenario.AmbulanceTeam(Position(24810, 248480)),
      new scenario.AmbulanceTeam(Position(148738, 268010)),
      new scenario.AmbulanceTeam(Position(324840, 48325)),
      new scenario.PoliceForce(Position(454848, 305548)),
      new scenario.PoliceForce(Position(68720, 218880)),
      new scenario.PoliceForce(Position(78148, 105870)),
      new scenario.PoliceForce(Position(123580, 38875))
*/    )

    scenario.rootEnsemble.init()
    println("System initialized")

    while (scenario.rootEnsemble.solve()) {
      println(scenario.toString)
    }

    // scenario.commit()
  }
}