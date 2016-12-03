package example

import mpmens.System
import mpmens.concerns.map2d.Position
import mpmens.model.Component

class RescueSystem(val universe: List[Component]) extends System {
  import implicits._

  class RescueEnsemble(incident: Incident) extends Ensemble[Incident]("rescue", incident) {
    val ambulances = role("ambulances", universe.withRole[Ambulance])

    membership(
      ambulances.cardinality == 2 &&
      ambulances.all(_.position.distanceTo(incident.position) <= 4) &&
      ambulances.all(x => ambulances.all(y => x.position.distanceTo(y.position) <= 4))
    )

    utility(ambulances.sum((x: Ambulance) => 100 - x.position.distanceTo(incident.position).round.toInt))
  }

  val rescueTeams = ensembles(universe.withRole[Incident] map(new RescueEnsemble(_)))

  utility(rescueTeams.sum(_.utility))
}


object Main {

  def main(args: Array[String]): Unit = {

    val universe = List(
      Ambulance(Position(3, 4)),
      Ambulance(Position(7, 3)),
      Ambulance(Position(5, 7)),
      Ambulance(Position(2, 4)),
      Ambulance(Position(3, 5)),
      Ambulance(Position(6, 7)),
      Incident(Position(4, 5)),
      Incident(Position(5, 6))
    )

    val system = new RescueSystem(universe)

    println("System instantiated")

    system.init()

    println("System initialized")

    while (system.solve()) {
//      println(s"Solution: ${ens1.utility} + ${ens2.utility} = ${system.totalUtility}\n" + system.toString())
      println(s"Solution:\n" + system.toString())
    }

  }

}
