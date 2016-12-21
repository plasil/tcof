package rcrs

import mpmens.Universe
import mpmens.traits.map2d.{Map2DTrait, Position}
import rcrs.traits.map2d.RCRSMapAdapterTrait
import rcrs.traits.time.CurrentTimeTrait
import rescuecore2.log.Logger
import rescuecore2.messages.Command
import rescuecore2.standard.entities.{Building, StandardEntityURN}
import rescuecore2.worldmodel.ChangeSet

class CentralAgent extends ScalaAgent {
  override type AgentEntityType = Building

  object RescueScenario extends Universe with RCRSAgentTrait with Map2DTrait with RCRSMapAdapterTrait with CurrentTimeTrait {

    class ExplorationTeams(val zone: MapZone) extends Ensemble(s"ExplorationTeam for $zone") {
      val fireBrigades = role("fireBrigades", components.withRole[FireBrigade])
      val ambulanceTeams = role("ambulanceTeams", components.withRole[AmbulanceTeam])
      val policeForces = role("policeForces", components.withRole[PoliceForce])

      membership(
        fireBrigades.cardinality >= 1 &&
        ambulanceTeams.cardinality >= 1 &&
        policeForces.cardinality >= 1
      )

      def proximityToZoneCenter(unit: MobileUnit) = 100 - (unit.position.distanceTo(zone.center) / 10000).round.toInt

      utility = fireBrigades.sum(unit => proximityToZoneCenter(unit)) +
        ambulanceTeams.sum(unit => proximityToZoneCenter(unit)) +
        policeForces.sum(unit => proximityToZoneCenter(unit))
    }

    system {
      val mapZones = for {
        xIdx <- 0 until 2
        yIdx <- 0 until 2
      } yield new MapZone(map, xIdx, yIdx, time - 20)

      val explorationTeams = ensembles("explorationTeams", mapZones.map(new ExplorationTeams(_)))

      membership(
        explorationTeams.map(team => team.fireBrigades).allDisjoint &&
        explorationTeams.map(team => team.ambulanceTeams).allDisjoint &&
        explorationTeams.map(team => team.policeForces).allDisjoint
      )
    }
  }


  override protected def postConnect() {
    Logger.info(s"Central agent connected")
    super.postConnect()
    RescueScenario.traitInit()
  }

  override def think(time: Int, changes: ChangeSet, heard: List[Command]): Unit = {
    Logger.info(s"CentralAgent: Think called at time $time")
    super.think(time, changes, heard)

    RescueScenario.rcrsTraitStep(time: Int, changes: ChangeSet, heard: List[Command])


    RescueScenario.components = List(
      new FireBrigade(Position(391738, 3370)),
      new FireBrigade(Position(424810, 354780)),
      new FireBrigade(Position(48738, 145870)),
      new FireBrigade(Position(187810, 248325)),
      new AmbulanceTeam(Position(128728, 82480)),
      new AmbulanceTeam(Position(24810, 248480)),
      new AmbulanceTeam(Position(148738, 268010)),
      new AmbulanceTeam(Position(324840, 48325)),
      new PoliceForce(Position(454848, 305548)),
      new PoliceForce(Position(68720, 218880)),
      new PoliceForce(Position(78148, 105870)),
      new PoliceForce(Position(123580, 38875))
    )

    RescueScenario.init()
    println("RescueScenario initialized")

    while (RescueScenario.solve()) {
      println(RescueScenario.toString)
    }

  }

  override protected def getRequestedEntityURNs: List[StandardEntityURN] = List(StandardEntityURN.FIRE_STATION, StandardEntityURN.AMBULANCE_CENTRE, StandardEntityURN.POLICE_OFFICE)
}
