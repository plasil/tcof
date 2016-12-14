package example

import mpmens.Universe
import mpmens.concerns.map2d.Position

object RescueScenario extends Universe {

  class IncidentResponseTeam(val incident: Incident) extends Ensemble("Incident Response Team for " + incident) {
    val ambulances = role("ambulances", components.withRole[Ambulance])

    membership(
      ambulances.cardinality >= 1 &&
      ambulances.all(_.position.distanceTo(incident.position) <= 5)
    )

    utility = ambulances.sum(100 - _.position.distanceTo(incident.position).round.toInt)
  }

  systems {
    val rescueTeams = ensembles("rescueTeams", components.withRole[Incident].map(new IncidentResponseTeam(_)))

    membership(
      rescueTeams.map(_.ambulances).allDisjoint
    )
  }
}


object Main {

  def main(args: Array[String]): Unit = {

    println("System instantiated")

    RescueScenario.components = List(
      Ambulance(Position(3, 4)),
      Ambulance(Position(7, 3)),
      Ambulance(Position(5, 7)),
      Ambulance(Position(2, 4)),
      Ambulance(Position(3, 5)),
      Ambulance(Position(6, 7)),
      Incident(Position(4, 5)),
      Incident(Position(5, 6))
    )

    RescueScenario.init()

    println("System initialized")

    while (RescueScenario.solve()) {

      val rescueTeams = RescueScenario.ensembles[RescueScenario.IncidentResponseTeam]("rescueTeams")
      for (team <- rescueTeams.selectedMembers) {
        println("RescueTeam " + team.name)
        for (ambulance <- team.ambulances.selectedMembers) {
          println(ambulance.position)
        }
      }

      println(RescueScenario.toString)
    }

  }

}
