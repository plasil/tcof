package example

import mpmens._
import mpmens.traits.map2d.{Position, PositionAware}

class TestScenario extends Universe {

  case class MapZone(xIdx: Int, yIdx: Int) {
    val center = new Position((xIdx + 0.5) * MapZone.xTileSize, (yIdx + 0.5) * MapZone.yTileSize)
  }

  object MapZone {
    val xTileSize = 250000
    val yTileSize = 200000
  }

  abstract class MobileUnit(var position: Position) extends Component with PositionAware {

    var areaExplorationAssignedZone: MapZone = _

    var shortId = -1

    val Register = State("Register")
    val AreaExploration = State("AreaExploration")
    val Stopped = State("Stopped")
    val Observation = State("Observation")

    val Operation = StateOr("Operation", Register, AreaExploration, Stopped)

    constraints(
      Operation
    )

    constraints(
      AreaExploration -> (areaExplorationAssignedZone != null) &&
      Register <-> (shortId == -1)
    )

    utility(
      states.sum {
        case Observation => 1
        case AreaExploration => 1
        case _ => 0
      }
    )

    preActions {
      println("preActions")
    }

    actions {
      states.selectedMembers.foreach {
        case AreaExploration => println("doExploration")
        case Register => println("doRegister")
        case _ =>
      }
    }

  }

  class PoliceForce(no: Int, _position: Position) extends MobileUnit(_position) {
    name(s"PoliceForce $no")
  }

  class AmbulanceTeam(no: Int, _position: Position) extends MobileUnit(_position) {
    name(s"AmbulanceTeam $no")
  }

  class FireBrigade(no: Int, _position: Position) extends MobileUnit(_position) {
    name(s"FireBrigade $no")
  }

  class ExplorationTeams(val zone: MapZone) extends Ensemble {
    name(s"ExplorationTeam for $zone")

    val mobileUnits = role("mobileUnits", components.select[MobileUnit])

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
      println("Actions 1")
      mobileUnits.foreachBySelection(_.areaExplorationAssignedZone = zone, _.areaExplorationAssignedZone = null)
    }
  }

  class System extends RootEnsemble {
    val mapZones = for {
      xIdx <- 0 until 1
      yIdx <- 0 until 2
    } yield new MapZone(xIdx, yIdx)

    val explorationTeams = ensembles("explorationTeams", mapZones.map(new ExplorationTeams(_)))

    membership(
      explorationTeams.map(_.fireBrigades).allDisjoint &&
      explorationTeams.map(_.ambulances).allDisjoint &&
      explorationTeams.map(_.police).allDisjoint
    )

    actions {
      println("Actions 2")
    }
  }

  val rootEnsemble = root(new System)
}

object TestScenario {
  def main(args: Array[String]): Unit = {
    val scenario = new TestScenario

    val fb = new scenario.FireBrigade(1, Position(391738, 3370))
    fb.shortId = -1
    fb.areaExplorationAssignedZone = new scenario.MapZone(0, 0)

    scenario.components = List(
      fb,
      new scenario.FireBrigade(2, Position(424810, 354780)),
//      new scenario.FireBrigade(3, Position(48738, 145870)),
//      new scenario.FireBrigade(4, Position(187810, 248325)),
//      new scenario.AmbulanceTeam(1, Position(128728, 82480)),
//      new scenario.AmbulanceTeam(2, Position(24810, 248480)),
//      new scenario.AmbulanceTeam(3, Position(148738, 268010)),
      new scenario.AmbulanceTeam(4, Position(324840, 48325)),
//      new scenario.PoliceForce(1, Position(454848, 305548)),
//      new scenario.PoliceForce(2, Position(68720, 218880)),
//      new scenario.PoliceForce(3, Position(78148, 105870)),
      new scenario.PoliceForce(4, Position(123580, 38875))
    )

    scenario.rootEnsemble.init()
    println("System initialized")

    while (scenario.rootEnsemble.solve()) {
      println(scenario.rootEnsemble.instance.toString)
    }

    scenario.rootEnsemble.commit()

/*
    fb.init()

    while (fb.solve()) {
      println(fb.toStringWithSolution)
    }

    fb.commit()
*/
  }

}
