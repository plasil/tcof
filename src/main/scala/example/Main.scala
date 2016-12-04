package example

import mpmens.System
import mpmens.concerns.map2d.Position

class RescueSystem() extends System {
  import implicits._

  class RescueEnsemble(incident: Incident) extends Ensemble[Incident]("rescue", incident) {
    val ambulances = role("ambulances", universe.withRole[Ambulance])

    membership(
      ambulances.cardinality == 3 &&
      ambulances.all(_.position.distanceTo(incident.position) <= 5) &&
      ambulances.all(x => ambulances.all(y => x.position.distanceTo(y.position) <= 8))
    )

    utility(ambulances.sum((x: Ambulance) => 100 - x.position.distanceTo(incident.position).round.toInt))
  }

  system {
    val rescueTeams = ensembles(universe.withRole[Incident] map (new RescueEnsemble(_)))

    membership {
      rescueTeams map (_.ambulances) allDisjoint
    }

    utility(rescueTeams.sum(_.utility))
  }
}


object Main {

  def main(args: Array[String]): Unit = {

    val system = new RescueSystem

    println("System instantiated")

    system.universe(List(
      Ambulance(Position(3, 4)),
      Ambulance(Position(7, 3)),
      Ambulance(Position(5, 7)),
      Ambulance(Position(2, 4)),
      Ambulance(Position(3, 5)),
      Ambulance(Position(6, 7)),
      Incident(Position(4, 5)),
      Incident(Position(5, 6))
    ))

    system.init()

    println("System initialized")

    while (system.solve()) {
      println(system.toString())
    }

  }

}
