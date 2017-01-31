package rcrs

import rcrs.comm._
import rcrs.scenario.RescueScenario
import rescuecore2.log.Logger
import rescuecore2.messages.Command
import rescuecore2.standard.entities.{Building, StandardEntity, StandardEntityURN}
import rescuecore2.standard.messages.AKSpeak
import rescuecore2.worldmodel.{ChangeSet, EntityID}
import tcof.traits.map2d.Position

import scala.collection.mutable


class CentralAgent extends ScalaAgent {
  override type AgentEntityType = Building

  case class AgentInfo(entity: StandardEntity, shortId: Int)

  val agentsById = mutable.Map.empty[EntityID, AgentInfo]
  val agentsByShortId = mutable.Map.empty[Int, AgentInfo]
  var shortIdCounter = 0

  val scenario = new RescueScenario


  override protected def postConnect() {
    Logger.info(s"Central agent connected")
    super.postConnect()

//    scenario.agent = this
    scenario.init()
  }


  override def think(time: Int, changes: ChangeSet, heard: List[Command]): Unit = {
    Logger.info(s"CentralAgent: Think called at time $time")
    super.think(time, changes, heard)

    if (time == ignoreAgentCommandsUntil) {
      Logger.info("Subscribing to channels")
      sendSubscribe(time, Constants.TO_STATION)
    }
///*
    import scenario.{map, map2dToRcrsMap2D}

    // TODO - mobileAgent has processing of heard messages inside preActions (e.g. Registration.preActions)
    // move it somewhere as well?
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
            val nodes = statusMap.map{ case (idx, status) => map.toNode(map.closeAreaIDs(referenceAreaId).byIdx(idx)) -> status}

            map.nodeStatus ++= nodes

            Logger.info(s"Exploration status updated for: ${nodes.keys.map(map.toArea)}")

          case _ =>
        }
      }

    }

    scenario.rcrsStep(time: Int, changes: ChangeSet, heard: List[Command])

    // TODO - only "mock" - positions of agent change during the simulation
    // TODO - add CentralAgent to components?
    scenario.components = List(
      new scenario.FireBrigade(0, Position(391738, 3370)),
      new scenario.FireBrigade(1, Position(424810, 354780)),
      new scenario.FireBrigade(2, Position(48738, 145870)),
      new scenario.FireBrigade(3, Position(187810, 248325))
//      new scenario.AmbulanceTeam(Position(128728, 82480)),
//      new scenario.AmbulanceTeam(Position(24810, 248480)),
//      new scenario.AmbulanceTeam(Position(148738, 268010)),
//      new scenario.AmbulanceTeam(Position(324840, 48325)),
//      new scenario.PoliceForce(Position(454848, 305548)),
//      new scenario.PoliceForce(Position(68720, 218880)),
//      new scenario.PoliceForce(Position(78148, 105870)),
//      new scenario.PoliceForce(Position(123580, 38875))
    )

    // TODO - central agent now handles whole rootEnsemble, is that correct?
    // In rcrs there may be multiple central agents
    scenario.rootEnsemble.init()
    println("RescueScenario initialized")

    while (scenario.rootEnsemble.solve()) {
      println(scenario.toString)
    }

    // TODO - ensemble sets zone, but this happens only on central agent.
    // Where is the message sent to mobile agent?
    scenario.rootEnsemble.commit()
//*/
  }

  override protected def getRequestedEntityURNs: List[StandardEntityURN] = List(
    StandardEntityURN.FIRE_STATION
//    StandardEntityURN.AMBULANCE_CENTRE,
//    StandardEntityURN.POLICE_OFFICE
  )
}
