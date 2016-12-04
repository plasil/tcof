package example

import mpmens.Universe
import mpmens.concerns.map2d.Position

class RescueScenario extends Universe {
  import implicits._

  class CityRescueSystem(name: String) extends Ensemble("City Rescue System " + name) {
    val all = role("all", components)

    class IncidentResponseTeam(val incident: Incident) extends Ensemble("Incident Response Team for " + incident) {
      val ambulances = role("ambulances", all.withRole[Ambulance])

      membership(
        all.contains(incident) &&
        ambulances.cardinality >= 1 &&
        ambulances.all(_.position.distanceTo(incident.position) <= 5) &&
        ambulances.all(x => ambulances.all(y => x.position.distanceTo(y.position) <= 8))
      )

      utility = ambulances.sum(100 - _.position.distanceTo(incident.position).round.toInt)
    }

    val rescueTeams = ensembles(all.withRole[Incident].map(new IncidentResponseTeam(_)))

    membership(
      all.cardinality == components.size / 2 &&
      rescueTeams.map(_.ambulances).allDisjoint
    )

    utility = rescueTeams.sum(_.utility)
  }

  systems {
    val systems = ensembles(new CityRescueSystem("#1"), new CityRescueSystem("#2"))

    membership(
      systems.map(_.all).allDisjoint
    )
  }
}


object Main {

  def main(args: Array[String]): Unit = {

    val scenario = new RescueScenario

    println("System instantiated")

    scenario.components = List(
      Ambulance(Position(3, 4)),
      Ambulance(Position(7, 3)),
      Ambulance(Position(5, 7)),
      Ambulance(Position(2, 4)),
      Ambulance(Position(3, 5)),
      Ambulance(Position(6, 7)),
      Incident(Position(4, 5)),
      Incident(Position(5, 6))
    )

    scenario.init()

    println("System initialized")

    while (scenario.solve()) {
      println(scenario.toString())
    }


    scenario.components = List(
      Ambulance(Position(3, 4)),
      Ambulance(Position(7, 3)),
      Ambulance(Position(5, 7)),
      Ambulance(Position(2, 4)),
      Ambulance(Position(3, 5)),
      Ambulance(Position(6, 7)),
      Incident(Position(5, 6))
    )

    scenario.init()

    println("System reinitialized")

    while (scenario.solve()) {
      println(scenario.toString())
    }

  }

}
