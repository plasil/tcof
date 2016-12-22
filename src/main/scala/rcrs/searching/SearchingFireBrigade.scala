package rcrs.searching

import rcrs.ScalaAgent
import rcrs.searching.SearchingMessage.Kind

import rescuecore2.log.Logger
import rescuecore2.messages.Command
import rescuecore2.standard.entities._
import rescuecore2.standard.messages.AKSpeak
import rescuecore2.worldmodel.{ChangeSet, EntityID}

import scala.collection.JavaConverters._

/**
  * A sample fire brigade agent.
  */
class SearchingFireBrigade extends ScalaAgent {
  override type AgentEntityType = FireBrigade

  private var maxWater: Int = 0
  private var maxDistance: Int = 0
  private var maxPower: Int = 0
  private var isKnown: Boolean = false
  private var winfo: AgentWorldInfo = new AgentWorldInfo

  override def toString = "Searching fire brigade"

  override protected def postConnect() {
    super.postConnect()

    Logger.info(s"Searching fire station connected")
  }

  override def think(time: Int, changes: ChangeSet, heard: List[Command]): Unit = {
    if (time == config.getIntValue(kernel.KernelConstants.IGNORE_AGENT_COMMANDS_KEY)) {
      sendSubscribe(time, Constants.CHANNELTOSTATION, Constants.CHANNELTOAGENTS)
    }
    val me: FireBrigade = this.me
    Logger.debug("Heard: " + heard.size)
    for (next <- heard) {
      next match {
        case speak: AKSpeak =>
          if (speak.getChannel == Constants.CHANNELTOAGENTS) {
            val msg: SearchingMessage = SearchingMessage.fromBytes(speak.getContent)
            if (!isKnown && msg.kind == Kind.ACK && ((msg.content.asInstanceOf[EntityID]) == me.getID)) {
              isKnown = true
              Logger.debug("Station confirmed me")
            }
          }
        case _ =>
      }
    }

    if (!isKnown) {
      Logger.debug("Sending ME")
      sendSpeak(time, Constants.CHANNELTOSTATION, SearchingMessage.newME.getBytes)
    }

    if (me.isWaterDefined && me.getWater < maxWater && location.isInstanceOf[Refuge]) {
      Logger.info("Filling with water at " + location)
      sendRest(time)
      return
    }

    if (me.isWaterDefined && me.getWater == 0) {
      // TODO - search
      //var path = search.breadthFirstSearch(me.getPosition, refugeIDs)
      var path: List[EntityID] = null
      if (path != null) {
        Logger.info("Moving to refuge")
        sendMove(time, path)
        return
      }
      else {
        Logger.debug("Couldn't plan a path to a refuge.")
        // TODO - randomWalk
        //path = randomWalk
        path = null
        Logger.info("Moving randomly")
        sendMove(time, path)
        return
      }
    }
    val all = getBurningBuildings
    all.filter(!winfo.sentBuildingsOnFire.contains(_)).foreach(e =>
      sendSpeak(time, Constants.CHANNELTOSTATION, SearchingMessage.newONFIRE(e).getBytes)
    )

    for (next <- all) {
      if (model.getDistance(getID, next) <= maxDistance) {
        Logger.info("Extinguishing " + next)
        sendExtinguish(time, next, maxPower)
        return
      }
    }
    for (next <- all) {
      val path = planPathToFire(next)
      if (path != null) {
        Logger.info("Moving to target")
        sendMove(time, path)
        return
      }
    }
    var path: List[EntityID] = null
    Logger.debug("Couldn't plan a path to a fire.")

    // TODO - add random walk instead of resting
//    path = randomWalk
//    Logger.info("Moving randomly")
//    sendMove(time, path)
    Logger.debug("Resting.")
    sendRest(time)
  }

  private def getBurningBuildings: List[EntityID] = {
    val buildings = model.getEntitiesOfType(StandardEntityURN.BUILDING).asScala

    val buildingsOnFire = buildings.collect {
      case b: Building if b.isOnFire => b
    }

    // TODO - logging
    buildingsOnFire.toList.sorted(new DistanceOrdering(location, model)).map(_.getID)
  }

  private def planPathToFire(target: EntityID): List[EntityID] = {
    val targets = model.getObjectsInRange(target, maxDistance)
    if (targets.isEmpty) {
      return null
    }
    // TODO - search
    //return search.breadthFirstSearch(me.getPosition, objectsToIDs(targets))
    return null
  }

  /**
    * Construct a random walk starting from this agent's current location to a random building.
    * Taken from AbstractAgent.java
    * TODO - should be probably in some trait/superclass
    *
    * @return A random walk.
    */
  protected def randomWalk: List[EntityID] = {
    // arraylist - vysledek
    // hashset seen
    val current = me.getPosition
    // at most RANDOM_WALK_LENGTH steps


//    val result = new util.ArrayList[EntityID](RANDOM_WALK_LENGTH)
//    val seen: util.Set[EntityID] = new util.HashSet[EntityID]
//    var current: EntityID = (me.asInstanceOf[Human]).getPosition
//    {
//      var i: Int = 0
//      while (i < RANDOM_WALK_LENGTH) {
//        {
//          result.add(current)
//          seen.add(current)
//          val possible: util.List[EntityID] = new util.ArrayList[EntityID](neighbours.get(current))
//          Collections.shuffle(possible, random)
//          var found: Boolean = false
//          import scala.collection.JavaConversions._
//          for (next <- possible) {
//            if (seen.contains(next)) {
//              continue //todo: continue is not supported
//            }
//            current = next
//            found = true
//            break //todo: break is not supported
//          }
//          if (!found) {
//            break //todo: break is not supported
//          }
//        }
//        ({
//          i += 1; i
//        })
//      }
//    }
//    return result
    return null
  }

  override protected def getRequestedEntityURNs: List[StandardEntityURN] = List(StandardEntityURN.FIRE_BRIGADE)
}

object SearchingFireBrigade {
  private val MAX_WATER_KEY: String = "fire.tank.maximum"
  private val MAX_DISTANCE_KEY: String = "fire.extinguish.max-distance"
  private val MAX_POWER_KEY: String = "fire.extinguish.max-sum"

  private val RANDOM_WALK_LENGTH = 50
}