package rcrs

import mpmens.traits.map2d.{Map2DTrait, Position}
import rcrs.traits.map2d.RCRSMapAdapterTrait
import rescuecore2.log.Logger
import rescuecore2.messages.Command
import rescuecore2.standard.entities.{StandardEntityURN, FireBrigade => FireBrigadeEntity}
import rescuecore2.worldmodel.ChangeSet

class FireBrigadeAgent extends ScalaAgent with Map2DTrait with RCRSMapAdapterTrait {
  override type AgentEntityType = FireBrigadeEntity

  private val MAX_WATER_KEY = "fire.tank.maximum"
  private val MAX_DISTANCE_KEY = "fire.extinguish.max-distance"
  private val MAX_POWER_KEY = "fire.extinguish.max-sum"

  private var maxWater: Int = _
  private var maxDistance: Int = _
  private var maxPower: Int = _

  private var explorationPath: map.AreaExploration = _

  override protected def postConnect() {
    super.postConnect()

    maxWater = config.getIntValue(MAX_WATER_KEY)
    maxDistance = config.getIntValue(MAX_DISTANCE_KEY)
    maxPower = config.getIntValue(MAX_POWER_KEY)

    Logger.info(s"Fire brigade agent connected: max extinguish distance = $maxDistance, max power = $maxPower, max tank = $maxWater")

    val toExplore = map.nodes.filter(node => node.center.x >= 250000 && node.center.y >= 0 && node.center.x < 500000 && node.center.y <= 150000)

    explorationPath = map.AreaExploration(map.currentNode, toExplore.toSet)
  }


  val assumeLen = 10
  var path: List[map.Node] = null
  var pathOrigin: map.Node = null

  override def think(time: Int, changes: ChangeSet, heard: List[Command]): Unit = {
    Logger.info(s"FireBrigadeAgent: Think called at time $time. Position ${getPosition}")
    super.think(time, changes, heard)

    if (time < ignoreAgentCommandsUntil) {
    } else {

      if (path != null) {
        val history = me.getPositionHistory.toList.grouped(2).collect{ case List(x,y) => Position(x,y)}.toList

        val walkedPath = map.getWalkedPath(pathOrigin, path, history)

        // Logger.info("Walked segment: " + walkedPath.map(map.toArea).toString)
        explorationPath.walked(walkedPath)
      }

      path = explorationPath.explorationPath
      pathOrigin = map.currentNode
      // Logger.info("Current node: " + map.toArea(pathOrigin))

      val assumePath = path.slice(0,assumeLen)

      // Logger.info("Assuming segment: " + assumePath.map(map.toArea).toString)
      explorationPath.assume(assumePath)

      // Logger.info("Walking path: " + path.map(map.toArea).toString)
      sendMove(time, map.toAreaID(path))
    }
  }

  override protected def getRequestedEntityURNs: List[StandardEntityURN] = List(StandardEntityURN.FIRE_BRIGADE)
}
