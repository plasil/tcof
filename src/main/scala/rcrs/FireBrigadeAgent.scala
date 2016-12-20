package rcrs

import mpmens.concerns.map2d.{Node, Position}
import rescuecore2.log.Logger
import rescuecore2.messages.Command
import rescuecore2.standard.entities.{FireBrigade, StandardEntityURN}
import rescuecore2.worldmodel.ChangeSet

class FireBrigadeAgent extends ScalaAgent {
  override type AgentEntityType = FireBrigade

  private val MAX_WATER_KEY = "fire.tank.maximum"
  private val MAX_DISTANCE_KEY = "fire.extinguish.max-distance"
  private val MAX_POWER_KEY = "fire.extinguish.max-sum"

  private var maxWater: Int = _
  private var maxDistance: Int = _
  private var maxPower: Int = _

  private var path: List[Node] = null

  override protected def postConnect() {
    super.postConnect()

    maxWater = config.getIntValue(MAX_WATER_KEY)
    maxDistance = config.getIntValue(MAX_DISTANCE_KEY)
    maxPower = config.getIntValue(MAX_POWER_KEY)

    Logger.info(s"Fire brigade agent connected: max extinguish distance = $maxDistance, max power = $maxPower, max tank = $maxWater")

  }

  override def think(time: Int, changes: ChangeSet, heard: List[Command]): Unit = {
    Logger.info(s"FireBrigadeAgent: Think called at time $time")

    if (time < ignoreAgentCommandsUntil) {

    } else {

      if (path == null) {
        Logger.info("Computing path")
        path = map.getExplorePath(map.currentNode, Position(250000, 0), Position(500000, 150000))
        Logger.info("Path done")
        sendMove(time, path)
      } else {
        val currentNode = map.currentNode
        val currentNodeIdx = path.indexOf(currentNode)

        // val pos = me.getPositionHistory.toList.grouped(2).collect{ case List(x,y) => Position(x,y)}.toList

        path = path.slice(currentNodeIdx + 1, path.size)

        sendMove(time, path)
      }
    }
  }

  override protected def getRequestedEntityURNs: List[StandardEntityURN] = List(StandardEntityURN.FIRE_BRIGADE)
}
