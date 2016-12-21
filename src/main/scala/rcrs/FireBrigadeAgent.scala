package rcrs

import mpmens.traits.map2d.Map2DTrait
import rcrs.traits.map2d.RCRSMapAdapterTrait
import rescuecore2.log.Logger
import rescuecore2.messages.Command
import rescuecore2.standard.entities.{StandardEntityURN, FireBrigade => FireBrigadeEntity}
import rescuecore2.worldmodel.ChangeSet

import scala.collection.mutable

class FireBrigadeAgent extends ScalaAgent with Map2DTrait with RCRSMapAdapterTrait {
  override type AgentEntityType = FireBrigadeEntity

  private val MAX_WATER_KEY = "fire.tank.maximum"
  private val MAX_DISTANCE_KEY = "fire.extinguish.max-distance"
  private val MAX_POWER_KEY = "fire.extinguish.max-sum"

  private var maxWater: Int = _
  private var maxDistance: Int = _
  private var maxPower: Int = _

  private var explorationPath: map.AreaExploration = _

  private val pathSoFar = mutable.ListBuffer.empty[map.Node]

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

  override def think(time: Int, changes: ChangeSet, heard: List[Command]): Unit = {
    Logger.info(s"FireBrigadeAgent: Think called at time $time. Position ${getPosition}")
    super.think(time, changes, heard)
/*
    if (time < ignoreAgentCommandsUntil) {
    } else {
      if (path != null) {
        val currentNode = map.currentNode
        val currentNodeIdx = path.indexOf(currentNode)
        val walkedPath = path.slice(0, currentNodeIdx + 1)

        Logger.info("Walked path: " + walkedPath.map(map.toArea).toString)

        // real path from history:
        // note - history of coordinates from getPositionHistory may skip some Area
        // therefore it's not possible to simply remove the beginning of the path list
        /*
        val coords = me.getPositionHistory.toList.grouped(2).collect{ case List(x,y) => Position(x,y)}.toList
        val areasFromCoords = coords.map(p => Map2D.coordinatesToArea(p.x.asInstanceOf[Int], p.y.asInstanceOf[Int], this.model).get)
        val areasWithoutDuplicates = Map2D.compress(areasFromCoords)
        Logger.info("Real   path: " + areasFromCoords.toString)
        Logger.info("Real (compressed) path: " + areasWithoutDuplicates.toString)
        */

        explorationPath.walked(walkedPath)
      }

      path = explorationPath.explorationPath
      Logger.info("Path to go: " + path.map(map.toArea).toString)

      val assumePath = path.slice(0,assumeLen)
      Logger.info("Assuming segment: " + path.map(map.toArea).toString)

      explorationPath.assume(path.slice(0,assumeLen))

      sendMove(time, map.toAreaID(path))
    }
*/
  }

  override protected def getRequestedEntityURNs: List[StandardEntityURN] = List(StandardEntityURN.FIRE_BRIGADE)
}
