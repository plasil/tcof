package rcrs.searching

import rcrs.ScalaAgent
import rescuecore2.log.Logger
import rescuecore2.messages.Command
import rescuecore2.standard.entities.{FireStation, StandardEntity, StandardEntityURN}
import rescuecore2.standard.messages.AKSpeak
import rescuecore2.worldmodel.{ChangeSet, EntityID}

import scala.collection.mutable.HashSet


class SearchingFireStation extends ScalaAgent {
  override type AgentEntityType = FireStation

  override def toString = "Searching fire station"

  override protected def postConnect() {
    super.postConnect()

    Logger.info(s"Searching fire station connected")
  }

  //val knownFireBrigades = HashSet[StandardEntity]()
  val knownFireBrigades = HashSet[StandardEntity]()
  val winfo = new StationWorldInfo()

  override def think(time: Int, changes: ChangeSet, heard: List[Command]): Unit = {
    Logger.info(s"CentralAgent: Think called at time $time")

    if (time == ignoreAgentCommandsUntil) {
      sendSubscribe(time, Constants.CHANNELTOSTATION, Constants.CHANNELTOAGENTS)
    }

    Logger.debug("All FB: " + model.getEntitiesOfType(StandardEntityURN.FIRE_BRIGADE).size)

    Logger.debug("Heard: " + heard.size)
    for (next <- heard) {
      next match {
        case speak: AKSpeak =>
          if (speak.getChannel == Constants.CHANNELTOSTATION) {
            val sender: StandardEntity = model.getEntity(speak.getAgentID)
            if (sender.getStandardURN == StandardEntityURN.FIRE_BRIGADE) {
              val msg: SearchingMessage = SearchingMessage.fromBytes(speak.getContent)
              msg.kind match {
                case SearchingMessage.Kind.ME =>
                  if (knownFireBrigades.contains(sender)) {
                    Logger.debug("Already know agent " + sender.getID)
                  }
                  else {
                    knownFireBrigades.add(sender)
                  }
                  sendSpeak(time, Constants.CHANNELTOAGENTS, SearchingMessage.newACK(sender.getID).getBytes)
                  Logger.debug("ACK to agent " + sender.getID)
                case SearchingMessage.Kind.ONFIRE =>
                  val key: EntityID = msg.content.asInstanceOf[EntityID]
                  winfo.buildingsOnFire.put(key, time)
                  Logger.debug("received burning building " + key)
                case _ =>
              }
            }
          }
        case _ =>
      }
    }

    sendRest(time)
  }

  override protected def getRequestedEntityURNs: List[StandardEntityURN] = List(StandardEntityURN.FIRE_STATION, StandardEntityURN.AMBULANCE_CENTRE, StandardEntityURN.POLICE_OFFICE)
}
