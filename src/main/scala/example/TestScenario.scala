package example

import mpmens._
import mpmens.traits.map2d.{Map2DTrait, Node}
import mpmens.traits.statespace.{StateSpaceTrait, interpolate}

object MapNodeKind extends Enumeration {
  type MapNodeKind = Value
  val Road, Building = Value
}

import example.MapNodeKind.{Building, MapNodeKind}

class MapNodeStatus(var kind: MapNodeKind, var isBurning: Boolean, var burnoutLevel: Double)

class TestScenario extends Universe with Map2DTrait[MapNodeStatus] with StateSpaceTrait {

  type MapNode = Node[MapNodeStatus]

  def burnModel(node: MapNode) = interpolate.linear(
    0.0 -> 0.0,
    0.5 -> 0.1,
    1.0 -> 1.0
  )

  abstract class MobileUnit extends Component {
    var mapPosition: MapNode = _
  }

  class AmbulanceTeam(id: Int) extends MobileUnit {
    name(s"AmbulanceTeam $id")
  }

  class FireBrigade(id: Int) extends MobileUnit {
    name(s"FireBrigade $id")
  }


  class FireFightingTeam(val building: MapNode) extends Ensemble {
    name(s"FireFightingTeam for building $building")

    val allMembers = role("mobileUnits", components.select[MobileUnit])
    val fireBrigades = role("fireBrigades", allMembers.selectEquiv[FireBrigade])
    val ambulances = role("ambulances", allMembers.selectEquiv[AmbulanceTeam])

    val routesToBuilding = map.shortestPath.to(building)
    val buildingBurnModel = statespace(burnModel(building), 0, building.status.burnoutLevel)

    membership(
      allMembers.all(unit => routesToBuilding.costFrom(unit.mapPosition) match {
        case None => false
        case Some(timeToBuilding) => buildingBurnModel.valueAt(timeToBuilding) < 0.9
      }) &&
      fireBrigades.cardinality >= 1 && fireBrigades.cardinality <= 3 && ambulances.cardinality === 1
    )

    def routeTimeToUtility(routeTime: Option[Double]) = routeTime match {
      case None => 0
      case Some(time) => 100 - (time / 10000).round.toInt
    }

    utility(
      allMembers.sum(unit => routeTimeToUtility(routesToBuilding.costFrom(unit.mapPosition)))
    )
  }

  class System extends RootEnsemble {
    val fireFightingTeams = ensembles("FireFightingTeams",
      map.nodes.filter(node => node.status.kind == Building && node.status.isBurning)
        .map(node => new FireFightingTeam(node))
    )

    membership(
      fireFightingTeams.map(_.allMembers).allDisjoint
    )
  }

  val rootEnsemble = root(new System)
}

object TestScenario {
  def main(args: Array[String]): Unit = {
    val scenario = new TestScenario
    scenario.init()

    scenario.components = List(
      new scenario.FireBrigade(1),
      new scenario.FireBrigade(2),
      new scenario.FireBrigade(3),
      new scenario.AmbulanceTeam(1),
      new scenario.AmbulanceTeam(2)
    )

    scenario.rootEnsemble.init()
    println("System initialized")

    while (scenario.rootEnsemble.solve()) {
      println(scenario.rootEnsemble.instance.toString)
    }

    scenario.rootEnsemble.commit()

    println(scenario.rootEnsemble.instance.solutionUtility)
  }

}

