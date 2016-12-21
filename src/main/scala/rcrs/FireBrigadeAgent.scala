package rcrs

import mpmens.concerns.map2d.{Node, Position}
import rescuecore2.log.Logger
import rescuecore2.messages.Command
import rescuecore2.standard.entities.{FireBrigade, StandardEntityURN}
import rescuecore2.worldmodel.ChangeSet

import scala.collection.mutable

class FireBrigadeAgent extends ScalaAgent {
  override type AgentEntityType = FireBrigade

  private val MAX_WATER_KEY = "fire.tank.maximum"
  private val MAX_DISTANCE_KEY = "fire.extinguish.max-distance"
  private val MAX_POWER_KEY = "fire.extinguish.max-sum"

  private var maxWater: Int = _
  private var maxDistance: Int = _
  private var maxPower: Int = _

  private var explorationPath: map.AreaExploration = _

  private val pathSoFar = mutable.ListBuffer.empty[Node]

  override protected def postConnect() {
    super.postConnect()

    maxWater = config.getIntValue(MAX_WATER_KEY)
    maxDistance = config.getIntValue(MAX_DISTANCE_KEY)
    maxPower = config.getIntValue(MAX_POWER_KEY)

    Logger.info(s"Fire brigade agent connected: max extinguish distance = $maxDistance, max power = $maxPower, max tank = $maxWater")

    explorationPath = map.areaExploration(map.currentNode, Position(250000, 0), Position(500000, 150000))
  }

  val assumeLen = 10
  var path: List[Node] = null

  override def think(time: Int, changes: ChangeSet, heard: List[Command]): Unit = {
    Logger.info(s"FireBrigadeAgent: Think called at time $time")

    if (time < ignoreAgentCommandsUntil) {
    } else {
      if (path != null) {
        val currentNode = map.currentNode
        val currentNodeIdx = path.indexOf(currentNode)
        val walkedPath = path.slice(0, currentNodeIdx + 1)

        Logger.info("Walked path: " + walkedPath.map(map.toArea).toString)

        explorationPath.walked(walkedPath)
      }

      path = explorationPath.explorationPath
      Logger.info("Path to go: " + path.map(map.toArea).toString)

      val assumePath = path.slice(0,assumeLen)
      Logger.info("Assuming segment: " + path.map(map.toArea).toString)

      explorationPath.assume(path.slice(0,assumeLen))

      sendMove(time, path)
    }
  }

  override protected def getRequestedEntityURNs: List[StandardEntityURN] = List(StandardEntityURN.FIRE_BRIGADE)
}
