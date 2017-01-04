package rcrs

import mpmens.Universe
import mpmens.traits.map2d.{Map2DTrait, Position, PositionAware}
import rcrs.comm._
import rcrs.traits.map2d.RCRSNodeStatus
import rcrs.traits.time.CurrentTimeTrait
import rescuecore2.log.Logger
import rescuecore2.messages.Command
import rescuecore2.standard.entities.{Building, StandardEntity, StandardEntityURN}
import rescuecore2.standard.messages.AKSpeak
import rescuecore2.worldmodel.{ChangeSet, EntityID}

import scala.collection.mutable

class CentralAgent extends ScalaAgent {
  override type AgentEntityType = Building

  case class AgentInfo(entity: StandardEntity, shortId: Int)

  val agentsById = mutable.Map.empty[EntityID, AgentInfo]
  val agentsByShortId = mutable.Map.empty[Int, AgentInfo]
  var shortIdCounter = 0

  class Mode

  object RescueScenario extends Universe with RCRSAgentTrait with Map2DTrait[RCRSNodeStatus] with CurrentTimeTrait {

    trait Explorer {
      var explorationZone: MapZone = null
    }

    abstract class MobileUnit(name: String, var position: Position) extends Component(name) with PositionAware with Explorer

    class PoliceForce(_position: Position) extends MobileUnit("PoliceForce", _position)
    class FireBrigade(_position: Position) extends MobileUnit("FireBrigade", _position)
    class AmbulanceTeam(_position: Position) extends MobileUnit("AmbulanceTeam", _position)

/*
      modes(AgentRegistrationManager, NodeStatusManager)

      constraints(
        Exploration -> (explorationZone != null) &&
        allExclusive(Exploration, Rest)
      )

      utility =
        Exploration -> 5 +
        Rest -> 3

      actions {
        modes.selectedMembers.foreach {
          case Exploration => exploration
          case Rest => rest
        }
      }
*/

    class ExplorationTeams(val zone: MapZone) extends Ensemble(s"ExplorationTeam for $zone") {
      val mobileUnits = role("mobileUnits", components.select[MobileUnit])

      val fireBrigades = role("fireBrigades", mobileUnits.selectEquiv[FireBrigade])
      val ambulances = role("ambulanceTeams", mobileUnits.selectEquiv[AmbulanceTeam])
      val police = role("policeForces", mobileUnits.selectEquiv[PoliceForce])

      membership(
        fireBrigades.cardinality >= 1 &&
        ambulances.cardinality >= 1 &&
        police.cardinality >= 1
      )

      def proximityToZoneCenter(unit: MobileUnit) = 100 - (unit.position.distanceTo(zone.center) / 10000).round.toInt

      utility = mobileUnits.sum(proximityToZoneCenter(_))

      actions {
        mobileUnits.foreachBySelection(_.explorationZone = zone, _.explorationZone = null)
      }
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

    if (time == ignoreAgentCommandsUntil) {
      Logger.info("Subscribing to channels")
      sendSubscribe(time, Constants.TO_STATION)
    }

    import RescueScenario.{map, map2dToRcrsMap2D}

    if (time >= ignoreAgentCommandsUntil) {
      val helloAcks = mutable.ListBuffer.empty[EntityID]

      Logger.info("Heard: " + heard)
      for (speak <- heard.collect{ case speak: AKSpeak => speak }) {
        val msg = Message.decode(speak.getContent)

        val sender = model.getEntity(speak.getAgentID)

        msg match {
          case RegRequest() =>
            val id = speak.getAgentID

            val agentInfo = agentsById.get(id) match {
              case None =>
                val aInfo = new AgentInfo(sender, shortIdCounter)

                shortIdCounter = shortIdCounter + 1

                agentsById += id -> aInfo
                agentsByShortId += aInfo.shortId -> aInfo

                aInfo

              case Some(aInfo) =>
                aInfo
            }

            sendSpeak(time, Constants.TO_AGENTS, Message.encode(new RegResponse(id, agentInfo.shortId)))

          case ExplorationStatus(referenceAreaId, statusMap) =>
            val refNode = map.toNode(referenceAreaId)
            val nodes = statusMap.map(kv => map.closeNodes(refNode).byIdx(kv._1) -> kv._2)

            map.nodeStatus ++= nodes

            Logger.info(s"Exploration status updated for: ${nodes.keys.map(map.toArea)}")

          case _ =>
        }
      }

    }

    /*
        RescueScenario.rcrsTraitStep(time: Int, changes: ChangeSet, heard: List[Command])


        RescueScenario.components = List(
          new RescueScenario.FireBrigade(Position(391738, 3370)),
          new RescueScenario.FireBrigade(Position(424810, 354780)),
          new RescueScenario.FireBrigade(Position(48738, 145870)),
          new RescueScenario.FireBrigade(Position(187810, 248325)),
          new RescueScenario.AmbulanceTeam(Position(128728, 82480)),
          new RescueScenario.AmbulanceTeam(Position(24810, 248480)),
          new RescueScenario.AmbulanceTeam(Position(148738, 268010)),
          new RescueScenario.AmbulanceTeam(Position(324840, 48325)),
          new RescueScenario.PoliceForce(Position(454848, 305548)),
          new RescueScenario.PoliceForce(Position(68720, 218880)),
          new RescueScenario.PoliceForce(Position(78148, 105870)),
          new RescueScenario.PoliceForce(Position(123580, 38875))
        )

        RescueScenario.init()
        println("RescueScenario initialized")

        while (RescueScenario.solve()) {
          println(RescueScenario.toString)
        }

        RescueScenario.commit()
    */
  }

  override protected def getRequestedEntityURNs: List[StandardEntityURN] = List(StandardEntityURN.FIRE_STATION, StandardEntityURN.AMBULANCE_CENTRE, StandardEntityURN.POLICE_OFFICE)
}
