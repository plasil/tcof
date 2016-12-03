package example

import mpmens.System
import mpmens.concerns.map2d.Position
import mpmens.model.Component

class RescueSystem extends System {
  import implicits._

  var universe = List(
    new Ambulance(Position(3, 4)),
    new Ambulance(Position(7, 3)),
    new Ambulance(Position(5, 7)),
    new Ambulance(Position(2, 4)),
    new Ambulance(Position(3, 5)),
    new Ambulance(Position(6, 7)),
    new Incident(Position(4, 5)),
    new Incident(Position(5, 6))
  )

  class RescueEnsemble(incident: Incident) extends Ensemble[Incident](incident) {
    val ambulances = role("ambulances", universe.withRole[Ambulance])

    membership(
      ambulances.cardinality is 2,
      ambulances.all(_.position.distanceTo(incident.position) <= 4),
      ambulances.all(x => ambulances.all(y => x.position.distanceTo(y.position) <= 4))
    )

    utility(ambulances.sum(_.position.distanceTo(incident.position).round.toInt))
  }

  val rescueTeams = ensembles(universe.withRole[Incident] map(new RescueEnsemble(_)))


}


object Main {

  def main(args: Array[String]): Unit = {

    val system = new System

    val allCars = List(
      new Ambulance(Position(3, 4)),
      new Ambulance(Position(7, 3)),
      new Ambulance(Position(5, 7)),
      new Ambulance(Position(2, 4)),
      new Ambulance(Position(3, 5)),
      new Ambulance(Position(6, 7))
    )

    def createEnsemble(incident: Incident) = {
      import system.implicits._

      val ens = system.addEnsemble(incident)

      val cars = ens.addRole("cars", allCars)

      ens.membership(
        cars.cardinality is 2,
        cars.all(_.position.distanceTo(incident.position) <= 4),
        cars.all(x => cars.all(y => x.position.distanceTo(y.position) <= 4))
      )

      ens.utility(cars.sum(_.position.distanceTo(incident.position).round.toInt))

      ens
    }

    val ens1 = createEnsemble(new Incident(Position(4, 5)))
    val ens1Cars = ens1.role[Ambulance]("cars")

    val ens2 = createEnsemble(new Incident(Position(5, 6)))
    val ens2Cars = ens2.role[Ambulance]("cars")

    while (system.solve()) {
      println(s"Solution: ${ens1.utility} + ${ens2.utility} = ${system.totalUtility}\n" + system.toString())
//      println(s"Solution:\n" + system.toString())
    }

  }

}
