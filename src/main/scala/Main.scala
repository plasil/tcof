case class Position(x: Double, y: Double) {
  def distanceTo(other: Position) = math.sqrt(math.pow(x - other.x, 2) + math.pow(y - other.y, 2))
}

class Component

trait PositionAware {
  var position: Position
}

case class Car(var position: Position) extends Component with PositionAware
case class Incident(var position: Position) extends Component with PositionAware


object Main {

  def main(args: Array[String]): Unit = {

    val system = new EnsemblesSystem

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




/*
  def evaluateExp(): Unit = {
    val model = new Model("system")

    val ens = model.setVar("ensemble #x", Array.empty[Int], 0 to Car.values.size toArray)
    val ensCost = model.intVar("ensemble #x cost", 0, Int.MaxValue - 1)

    val cars = Car.values
    val incident = Position(4, 5)

    model.arithm(ens.getCard, "=", 2).reify()

    val cnd = LogOp.and(cars.map(x => LogOp.implies(model.member(x.idx, ens).reify, model.boolVar(x.position.distanceTo(incident.position) < 10))) toArray : _*)

    cnd match {
      case logOp: LogOp => model.addClauses(cnd)
      case boolVar: BoolVar => model.addClauseTrue(boolVar)
    }

    val cnd =
      LogOp.and(cars.map(x => LogOp.implies(
        model.member(x.idx, ens).reify,
        LogOp.or(cars.map(y => LogOp.implies(model.member(y.idx, ens).reify, model.boolVar(x.position.distanceTo(y.position) < 10))) toArray : _*)
      )) toArray : _*)


    model.sumElements(ens, Car.values map { _.position.distanceTo(incident).round.toInt } toArray, ensCost).post()




    val e2 = model.setVar("E2", Array.empty[Int], 0 to Car.values.size toArray)
    (e2.getCard === 2).post()
    val e2Cost = model.intVar("E2 cost", 0, Int.MaxValue - 1)
    model.sumElements(e2, Car.values map { _.position.distanceTo(incident2Position).round.toInt } toArray, e2Cost).post()

    model.allDisjoint(e1, e2).post()

    val totalCost = model.intVar("Total cost", 0, Int.MaxValue - 1)
    model.sum(Array(e1Cost, e2Cost), "=", totalCost).post()

    for (car <- Car.values) {
      model.ifThen(model.boolVar(!(car.position.distanceTo(incident1Position) <= maxDistance)), model.notMember(car.idx, e1))
    }

    for (car <- Car.values) {
      model.ifThen(model.boolVar(!(car.position.distanceTo(incident2Position) <= maxDistance)), model.notMember(car.idx, e2))
    }

    model.setObjective(Model.MINIMIZE, totalCost)

  }

  def main(args: Array[String]): Unit = {

    object Implicits {
      implicit class RichIntVar(iv: IntVar) {
        def ===(other: Int) = iv.eq(other)
      }
    }

    import Implicits._

    /* Construction of components */
    Car(3, 4)
    Car(7, 3)
    Car(5, 7)
    Car(2, 4)
    Car(3, 5)
    Car(6, 7)

    val incident1Position = Position(4, 5)
    val incident2Position = Position(5, 6)

    val maxDistance = 3

    val model = new Model("distance")

    val e1 = model.setVar("E1", Array.empty[Int], 0 to Car.values.size toArray)
    (e1.getCard === 2).post()
    val e1Cost = model.intVar("E1 cost", 0, Int.MaxValue - 1)
    model.sumElements(e1, Car.values map { _.position.distanceTo(incident1Position).round.toInt } toArray, e1Cost).post()

    val e2 = model.setVar("E2", Array.empty[Int], 0 to Car.values.size toArray)
    (e2.getCard === 2).post()
    val e2Cost = model.intVar("E2 cost", 0, Int.MaxValue - 1)
    model.sumElements(e2, Car.values map { _.position.distanceTo(incident2Position).round.toInt } toArray, e2Cost).post()

    model.allDisjoint(e1, e2).post()

    val totalCost = model.intVar("Total cost", 0, Int.MaxValue - 1)
    model.sum(Array(e1Cost, e2Cost), "=", totalCost).post()

    for (car <- Car.values) {
      model.ifThen(model.boolVar(!(car.position.distanceTo(incident1Position) <= maxDistance)), model.notMember(car.idx, e1))
    }

    for (car <- Car.values) {
      model.ifThen(model.boolVar(!(car.position.distanceTo(incident2Position) <= maxDistance)), model.notMember(car.idx, e2))
    }

    model.setObjective(Model.MINIMIZE, totalCost)

    while (model.getSolver().solve()) {
      println(s"$e1  $e2  $e1Cost  $e2Cost  $totalCost")
    }

  }
*/
