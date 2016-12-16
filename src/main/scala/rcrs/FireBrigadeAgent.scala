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

  private var path: List[Node] = _
  private var previousNode: Node = _
  private var freshPath = true

  override protected def postConnect() {
    super.postConnect()

    maxWater = config.getIntValue(MAX_WATER_KEY)
    maxDistance = config.getIntValue(MAX_DISTANCE_KEY)
    maxPower = config.getIntValue(MAX_POWER_KEY)

    Logger.info(s"Fire brigade agent connected: max extinguish distance = $maxDistance, max power = $maxPower, max tank = $maxWater")

    path = map.getExplorePath(map.currentNode, Position(250000, 0), Position(500000, 150000))
  }

  override def think(time: Int, changes: ChangeSet, heard: List[Command]): Unit = {
    if (time < ignoreAgentCommandsUntil) {

    } else {
//      val ffNode = map.currentNode
//      val hydrantNode = map.toNode(new EntityID(32781))
//      val pathOpt = map.getPath(ffNode, hydrantNode)

      if (!freshPath) {
        val currentNode = map.currentNode
        val currentNodeIdx = path.indexOf(currentNode)

        val pos = me.getPositionHistory.toList.grouped(2).collect{ case List(x,y) => Position(x,y)}.toList

        val traveledPath = previousNode :: path.slice(0, currentNodeIdx + 1)

        println("Hist pos: " + pos)
        println("Path pos: " + traveledPath.map(_.center))

        val traveledDistance = traveledPath.zip(traveledPath.tail).map(x => x._1.center.distanceTo(x._2.center)).sum
        println(s"Traveled [$currentNodeIdx] $traveledDistance")

        path = path.slice(currentNodeIdx + 1, path.size)
      }


      sendMove(time, path)
      freshPath = false
      previousNode = map.currentNode
    }
  }

  override protected def getRequestedEntityURNs: List[StandardEntityURN] = List(StandardEntityURN.FIRE_BRIGADE)
}
