package rcrs

import mpmens.traits.map2d.Map2DTrait
import rcrs.comm._
import rcrs.traits.map2d.{RCRSMapAdapterTrait, RCRSNodeStatus}
import rescuecore2.log.Logger
import rescuecore2.messages.Command
import rescuecore2.standard.entities.{StandardEntityURN, FireBrigade => FireBrigadeEntity}
import rescuecore2.worldmodel.ChangeSet


class FireBrigadeAgent extends ScalaAgent with Map2DTrait[RCRSNodeStatus] with RCRSMapAdapterTrait {
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
  }




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


    }
  }

  override protected def getRequestedEntityURNs: List[StandardEntityURN] = List(StandardEntityURN.FIRE_BRIGADE)
}
