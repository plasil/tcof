package rcrs.scenario

import rcrs.comm.{Constants, Message, RegRequest, RegResponse}
import rcrs.traits.RCRSConnectorTrait
import rescuecore2.log.Logger
import rescuecore2.standard.entities.StandardEntity
import rescuecore2.worldmodel.EntityID
import tcof.Universe

import scala.collection.mutable

trait RegistrationSupport {
  this: Universe with RCRSConnectorTrait =>

  /**
    * Obtains a shortId from the central server.
    */
  trait Registration {
    this: MobileUnitComponent#MobileUnit =>

    private var shortId = -1

    val Register = State

    preActions {
      sensing.messages.foreach{
        case (RegResponse(id, sId), _) if id == agent.getID =>
          shortId = sId
          Logger.info(s"Agent registered id: $id, shortId: $sId")

        case _ =>
      }
    }

    constraints(
      Register <-> (shortId == -1)
    )

    actions {
      states.selectedMembers.foreach {
        case Register => agent.sendSpeak(time, Constants.TO_STATION, Message.encode(new RegRequest()))
        case _ =>
      }
    }
  }

  /**
    * Assigns a shortId.
    */
  trait Registrator {
    this: CentralUnitComponent#CentralUnit =>

    case class AgentInfo(entity: StandardEntity, shortId: Int)

    val agentsById = mutable.Map.empty[EntityID, AgentInfo]
    val agentsByShortId = mutable.Map.empty[Int, AgentInfo]
    var shortIdCounter = 0

    preActions {
      sensing.messages.foreach {
        case (RegRequest(), speak) =>
          val id = speak.getAgentID

          val sender = agent.model.getEntity(speak.getAgentID)

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

          // TODO - is ok to send message in preactions? Or buffer message somewhere and send it in
          // actions?
          agent.sendSpeak(time, Constants.TO_AGENTS, Message.encode(new RegResponse(id, agentInfo.shortId)))

        case _ =>
      }
    }
  }
}
