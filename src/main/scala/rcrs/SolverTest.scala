package rcrs

import mpmens.traits.map2d.Position
import rcrs.scenario.RescueScenario

object SolverTest {
  def main(args: Array[String]): Unit = {
    val scenario = new RescueScenario

    scenario.components = List(
      new scenario.FireBrigade(1, Position(391738, 3370)),
      new scenario.FireBrigade(2, Position(424810, 354780)),
      new scenario.FireBrigade(3, Position(48738, 145870)),
      new scenario.FireBrigade(4, Position(187810, 248325)),
      new scenario.AmbulanceTeam(1, Position(128728, 82480)),
      new scenario.AmbulanceTeam(2, Position(24810, 248480)),
      new scenario.AmbulanceTeam(3, Position(148738, 268010)),
      new scenario.AmbulanceTeam(4, Position(324840, 48325)),
      new scenario.PoliceForce(1, Position(454848, 305548)),
      new scenario.PoliceForce(2, Position(68720, 218880)),
      new scenario.PoliceForce(3, Position(78148, 105870)),
      new scenario.PoliceForce(4, Position(123580, 38875))
    )

    /*
    scenario.rootEnsemble.init()
    println("System initialized")

    while (scenario.rootEnsemble.solve()) {
      println(scenario.rootEnsemble.instance.toString)
    }

    scenario.rootEnsemble.commit()
  */

    val fb = scenario.components.head
    fb.init()

    while (fb.solve()) {
      println(fb.toStringWithSolution)
    }

    fb.commit()


  }
}
