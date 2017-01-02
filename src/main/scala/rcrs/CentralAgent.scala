package rcrs

import mpmens.Universe
import mpmens.traits.map2d.Map2DTrait
import rcrs.AgentType.AgentType
import rcrs.comm.{Hello, HelloAck, Message}
import rcrs.searching.Constants
import rcrs.traits.time.CurrentTimeTrait
import rescuecore2.log.Logger
import rescuecore2.messages.Command
import rescuecore2.standard.entities.{Building, StandardEntityURN}
import rescuecore2.standard.messages.AKSpeak
import rescuecore2.worldmodel.{ChangeSet, EntityID}

import scala.collection.mutable

class CentralAgent extends ScalaAgent {
  override type AgentEntityType = Building

  case class AgentInfo(id: EntityID, agentType: AgentType)

  val agents = mutable.Map.empty[EntityID, AgentInfo]

  object RescueScenario extends Universe with RCRSAgentTrait with Map2DTrait with CurrentTimeTrait {

    class ExplorationTeams(val zone: MapZone) extends Ensemble(s"ExplorationTeam for $zone") {
      val fireBrigades = role("fireBrigades", components.withRole[FireBrigade])
      val ambulances = role("ambulanceTeams", components.withRole[AmbulanceTeam])
      val police = role("policeForces", components.withRole[PoliceForce])

      membership(
        fireBrigades.cardinality >= 1 &&
        ambulances.cardinality >= 1 &&
        police.cardinality >= 1
      )

      def proximityToZoneCenter(unit: MobileUnit) = 100 - (unit.position.distanceTo(zone.center) / 10000).round.toInt

      utility = fireBrigades.sum(proximityToZoneCenter(_)) +
        ambulances.sum(proximityToZoneCenter(_)) +
        police.sum(proximityToZoneCenter(_))
    }

    system {
      val mapZones = for {
        xIdx <- 0 until 2
        yIdx <- 0 until 2
      } yield new MapZone(map, xIdx, yIdx, time - 20)

      val explorationTeams = ensembles("explorationTeams", mapZones.map(new ExplorationTeams(_)))

      membership(
        explorationTeams.map(_.fireBrigades).allDisjoint &&
        explorationTeams.map(_.ambulances).allDisjoint &&
        explorationTeams.map(_.police).allDisjoint
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

    if (time < ignoreAgentCommandsUntil) {
      Logger.info("Subscribing to channels")
      sendSubscribe(time, Constants.CHANNELTOSTATION, Constants.CHANNELTOAGENTS)
    } else {

      val helloAcks = mutable.ListBuffer.empty[EntityID]

      Logger.info("Heard: " + heard)
      for (command <- heard) {
        command match {
          case speak: AKSpeak =>
            if (speak.getChannel == Constants.CHANNELTOAGENTS) {
              val msg = Message.decode(speak.getContent)

              msg match {
                case Hello(agentType) =>
                  println(speak.getURN)
                  helloAcks += speak.getAgentID
                  agents += speak.getAgentID -> new AgentInfo(speak.getAgentID, agentType)
                case _ =>
              }
            }

          case _ =>
        }
      }

    if (helloAcks.nonEmpty) {
      Logger.info("Sending HelloAck " + helloAcks)
      sendSpeak(time, Constants.CHANNELTOAGENTS, Message.encode(new HelloAck(helloAcks.toList)))
    }

    /*
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
    */
    }
  }

  override protected def getRequestedEntityURNs: List[StandardEntityURN] = List(StandardEntityURN.FIRE_STATION, StandardEntityURN.AMBULANCE_CENTRE, StandardEntityURN.POLICE_OFFICE)
}
