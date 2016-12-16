package rcrs

import rescuecore2.log.Logger
import rescuecore2.messages.Command
import rescuecore2.standard.entities.{Building, StandardEntityURN}
import rescuecore2.worldmodel.ChangeSet

class CentralAgent extends ScalaAgent {
  override type AgentEntityType = Building

  override protected def postConnect() {
    super.postConnect()

    Logger.info(s"Central agent connected")
  }

  override def think(time: Int, changes: ChangeSet, heard: List[Command]): Unit = {
    Logger.info(s"Think called at time $time")


  }

  override protected def getRequestedEntityURNs: List[StandardEntityURN] = List(StandardEntityURN.FIRE_STATION, StandardEntityURN.AMBULANCE_CENTRE, StandardEntityURN.POLICE_OFFICE)
}
