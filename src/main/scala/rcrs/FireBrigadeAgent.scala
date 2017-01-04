package rcrs

import mpmens.traits.map2d.{Map2DTrait, Position}
import rcrs.comm._
import rcrs.traits.map2d.{BuildingStatus, RCRSMapAdapterTrait, RCRSNodeStatus, RoadStatus}
import rescuecore2.log.Logger
import rescuecore2.messages.Command
import rescuecore2.standard.entities.{Area, Building, Road, StandardEntityURN, FireBrigade => FireBrigadeEntity}
import rescuecore2.standard.messages.AKSpeak
import rescuecore2.worldmodel.ChangeSet

import scala.collection.JavaConverters._
import scala.collection.mutable


class FireBrigadeAgent extends ScalaAgent with Map2DTrait[RCRSNodeStatus] with RCRSMapAdapterTrait {
  override type AgentEntityType = FireBrigadeEntity

  private val MAX_WATER_KEY = "fire.tank.maximum"
  private val MAX_DISTANCE_KEY = "fire.extinguish.max-distance"
  private val MAX_POWER_KEY = "fire.extinguish.max-sum"

  private var maxWater: Int = _
  private var maxDistance: Int = _
  private var maxPower: Int = _

  private var explorationPath: map.AreaExploration = _

  private var shortId = -1

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

    Logger.info(s"changes: $changes")

    if (time == ignoreAgentCommandsUntil) {
      Logger.info("Subscribing to channels")
      sendSubscribe(time, Constants.TO_AGENTS)
    }

    if (time >= ignoreAgentCommandsUntil) {
      Logger.info("Heard: " + heard)
      for (speak <- heard.collect{ case speak: AKSpeak => speak }) {
        val msg = Message.decode(speak.getContent)

        msg match {
          case RegResponse(id, sId) if id == getID =>
            shortId = sId
            Logger.info(s"Agent registered id: $id, shortId: $sId")

          case _ =>
        }
      }

      if (shortId == -1) {
        sendSpeak(time, Constants.TO_STATION, Message.encode(new RegRequest()))
      }


      val referenceNode = map.toNode(currentAreaId)
      val statusChanges = mutable.Map.empty[map.Node, RCRSNodeStatus]

      for (entityId <- changes.getChangedEntities.asScala) {

        agent.model.getEntity(entityId) match {
          case area: Area =>
            val changedNode = map.toNode(area.getID)

            area match {
              case road: Road => statusChanges += changedNode -> RoadStatus(42)
              case building: Building => statusChanges += changedNode -> BuildingStatus(changes.getChangedProperty(entityId, "urn:rescuecore2.standard:property:temperature").getValue.asInstanceOf[Int], changes.getChangedProperty(entityId, "urn:rescuecore2.standard:property:brokenness").getValue.asInstanceOf[Int])
            }

          case _ =>
        }
      }

      map.nodeStatus ++= statusChanges

      val msg = new ExplorationStatus(currentAreaId, statusChanges.collect{ case (node, status) => map.closeAreaIDs(currentAreaId).byAreaId(map.toArea(node).getID) -> status }.toMap)
      println(msg)
      sendSpeak(time, Constants.TO_STATION, Message.encode(msg))



      if (path != null) {
        val history = me.getPositionHistory.toList.grouped(2).map{ case List(x,y) => Position(x,y) }.toList

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
