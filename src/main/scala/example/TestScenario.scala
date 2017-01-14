package example

import tcof._
import tcof.traits.map2d.{Map2DTrait, Node}
import tcof.traits.statespace.{StateSpaceTrait, interpolate}
import tcof.traits.statistics.StatisticsTrait

object MapNodeKind extends Enumeration {
  type MapNodeKind = Value
  val Road, Building = Value
}

import example.MapNodeKind.{Building, MapNodeKind}

class MapNodeStatus(var kind: MapNodeKind, var isBurning: Boolean, var burnoutLevel: Double)

class TestScenario extends Universe with Map2DTrait[MapNodeStatus] with StateSpaceTrait with StatisticsTrait {

  type MapNode = Node[MapNodeStatus]

  var noOfStatusMsgReceived = 0
  var noOfStatusMsgToBeReceived = 1
  var time = 0

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

  val msgDelivery = timeseries.binomial(noOfStatusMsgReceived, noOfStatusMsgToBeReceived - noOfStatusMsgReceived)

  class FireFightingTeam(val building: MapNode) extends Ensemble {
    name(s"FireFightingTeam for building $building")
    val fireBrigades = role("fireBrigades", components.select[FireBrigade])
    val ambulances = role("ambulances", components.select[AmbulanceTeam])
    val allMembers = fireBrigades ++ ambulances

    val routesToBuilding = map.shortestPath.to(building)
    val firePredictor = statespace(burnModel(building), 0, building.status.burnoutLevel)

    membership(
      (msgDelivery(time - 3600, time).probability > 0.9 withConfidence 0.95) &&

      allMembers.all(unit => routesToBuilding.costFrom(unit.mapPosition) match {
        case None => false
        case Some(travelTime) => firePredictor.valueAt(travelTime) < 0.9
      }) &&

      fireBrigades.cardinality >= 1 && fireBrigades.cardinality <= 3 && ambulances.cardinality === 1
    )

    def travelTimeToUtility(routeTime: Option[Double]) = routeTime match {
      case None => 0
      case Some(time) => 100 - (time / 10000).round.toInt
    }

    utility(
      allMembers.sum(unit => travelTimeToUtility(routesToBuilding.costFrom(unit.mapPosition)))
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

