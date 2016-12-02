package example

import mpmens.System
import mpmens.concerns.map2d.Position

object Main {

  def main(args: Array[String]): Unit = {

    val system = new System

    val allCars = List(
      new Car(Position(3, 4)),
      new Car(Position(7, 3)),
      new Car(Position(5, 7)),
      new Car(Position(2, 4)),
      new Car(Position(3, 5)),
      new Car(Position(6, 7))
    )

    def createEnsemble(incident: Incident) = {
      import system.implicits._

      val ens = system.addEnsemble(incident)

      val cars = ens.addRole("cars", allCars)

      ens.ensure(
        cars.cardinality is 2,
        cars.all(_.position.distanceTo(incident.position) <= 4),
        cars.all(x => cars.all(y => x.position.distanceTo(y.position) <= 4))
      )

      ens.cost(cars.sum(_.position.distanceTo(incident.position).round.toInt))

      ens
    }

    val ens1 = createEnsemble(new Incident(Position(4, 5)))
    val ens1Cars = ens1.role[Car]("cars")

    val ens2 = createEnsemble(new Incident(Position(5, 6)))
    val ens2Cars = ens2.role[Car]("cars")

    while (system.solve()) {
      println(s"Solution: ${ens1.cost} + ${ens2.cost} = ${system.totalCost}\n" + system.toString())
//      println(s"Solution:\n" + system.toString())
    }

  }

}
