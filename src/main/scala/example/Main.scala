package example

import mpmens.System
import mpmens.concerns.map2d.Position
import mpmens.model.Component

class RescueSystem extends System {
  import implicits._

  var universe = List.empty[Component]

  class RescueEnsemble(incident: Incident) extends Ensemble[Incident]("rescue", incident) {
    val ambulances = role("ambulances", universe.withRole[Ambulance])

    membership(
      ambulances.cardinality == 2 &&
      ambulances.all(_.position.distanceTo(incident.position) <= 4) &&
      ambulances.all(x => ambulances.all(y => x.position.distanceTo(y.position) <= 4))
    )

    utility(ambulances.sum(_.position.distanceTo(incident.position).round.toInt))
  }

  val rescueTeams = ensembles(universe.withRole[Incident] map(new RescueEnsemble(_)))

  utility(rescueTeams.sum(_.utility))

}


object Main {

  def main(args: Array[String]): Unit = {

    val system = new RescueSystem

    system.universe = List(
      new Ambulance(Position(3, 4)),
      new Ambulance(Position(7, 3)),
      new Ambulance(Position(5, 7)),
      new Ambulance(Position(2, 4)),
      new Ambulance(Position(3, 5)),
      new Ambulance(Position(6, 7)),
      new Incident(Position(4, 5)),
      new Incident(Position(5, 6))
    )

    while (system.solve()) {
//      println(s"Solution: ${ens1.utility} + ${ens2.utility} = ${system.totalUtility}\n" + system.toString())
      println(s"Solution:\n" + system.toString())
    }

  }

}
