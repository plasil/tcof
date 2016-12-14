package rcrs

import java.util.{Collection, EnumSet}

import rescuecore2.log.Logger
import rescuecore2.messages.Command
import rescuecore2.standard.entities.{FireBrigade, StandardEntityURN}
import rescuecore2.worldmodel.ChangeSet

class FireBrigadeAgent extends ScalaAgent[FireBrigade] with WithMap2D {

  private val MAX_WATER_KEY = "fire.tank.maximum"
  private val MAX_DISTANCE_KEY = "fire.extinguish.max-distance"
  private val MAX_POWER_KEY = "fire.extinguish.max-sum"

  private var maxWater: Int = _
  private var maxDistance: Int = _
  private var maxPower: Int = _

  override protected def postConnect() {
    super.postConnect()

    maxWater = config.getIntValue(MAX_WATER_KEY)
    maxDistance = config.getIntValue(MAX_DISTANCE_KEY)
    maxPower = config.getIntValue(MAX_POWER_KEY)

    Logger.info(s"Fire brigade agent connected: max extinguish distance = $maxDistance, max power = $maxPower, max tank = $maxWater")

    me.getPosition()

  }

  override def think(time: Int, changes: ChangeSet, heard: Collection[Command]): Unit = {
    Logger.debug("think called")

  }

  override def getRequestedEntityURNsEnum: EnumSet[StandardEntityURN] = EnumSet.of(StandardEntityURN.FIRE_BRIGADE)
}
