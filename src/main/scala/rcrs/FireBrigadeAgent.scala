package rcrs

import rescuecore2.log.Logger
import rescuecore2.messages.Command
import rescuecore2.standard.entities.{FireBrigade, StandardEntityURN}
import rescuecore2.worldmodel.{ChangeSet, EntityID}

class FireBrigadeAgent extends ScalaAgent {
  override type AgentEntityType = FireBrigade

  private val MAX_WATER_KEY = "fire.tank.maximum"
  private val MAX_DISTANCE_KEY = "fire.extinguish.max-distance"
  private val MAX_POWER_KEY = "fire.extinguish.max-sum"

  private var maxWater: Int = _
  private var maxDistance: Int = _
  private var maxPower: Int = _

  override protected def postConnect() {
    maxWater = config.getIntValue(MAX_WATER_KEY)
    maxDistance = config.getIntValue(MAX_DISTANCE_KEY)
    maxPower = config.getIntValue(MAX_POWER_KEY)

    Logger.info(s"Fire brigade agent connected: max extinguish distance = $maxDistance, max power = $maxPower, max tank = $maxWater")

    map.populate()

  }

  override def think(time: Int, changes: ChangeSet, heard: List[Command]): Unit = {
    Logger.info(s"Think called at time $time")
    if (time < ignoreAgentCommandsUntil) {
    } else {
      val ffNode = map.toNode(me.getPosition)
      val hydrantNode = map.toNode(new EntityID(32781))
      val pathOpt = map.getPath(ffNode, hydrantNode)

      if (pathOpt.nonEmpty) {
        val path = pathOpt.get
        Logger.info("Path: " + path.map(map.toArea(_).getID))
        sendMove(time, path)
      }
    }
  }

  override protected def getRequestedEntityURNs: List[StandardEntityURN] = List(StandardEntityURN.FIRE_BRIGADE)
}
