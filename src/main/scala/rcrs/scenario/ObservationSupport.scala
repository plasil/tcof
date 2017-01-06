package rcrs.scenario

import mpmens.traits.map2d.{Map2DTrait, Node}
import rcrs.comm.{Constants, ExplorationStatus, Message}
import rcrs.traits.RCRSConnectorTrait
import rcrs.traits.map2d.{BuildingStatus, RCRSNodeStatus, RoadStatus}
import rcrs.traits.time.CurrentTimeTrait
import rescuecore2.standard.entities.{Area, Building, Road}

import scala.collection.JavaConverters._
import scala.collection.mutable

trait ObservationSupport {
  this: RCRSConnectorTrait with Map2DTrait[RCRSNodeStatus] with CurrentTimeTrait =>

  trait Observation {
    this: MobileUnitComponent#MobileUnit =>

    val Observation = State

    actions {
      states.selectedMembers.foreach {
        case Observation => doObservation()
        case _ =>
      }
    }

    def doObservation() {
      val referenceNode = map.toNode(agent.currentAreaId)
      val statusChanges = mutable.Map.empty[Node[RCRSNodeStatus], RCRSNodeStatus]

      val changes = sensing.changes

      for (entityId <- changes.getChangedEntities.asScala) {

        agent.model.getEntity(entityId) match {
          case area: Area =>
            val changedNode = map.toNode(area.getID)

            area match {
              case road: Road => statusChanges += changedNode -> RoadStatus(42 /* TODO */)
              case building: Building => statusChanges += changedNode -> BuildingStatus(changes.getChangedProperty(entityId, "urn:rescuecore2.standard:property:temperature").getValue.asInstanceOf[Int], changes.getChangedProperty(entityId, "urn:rescuecore2.standard:property:brokenness").getValue.asInstanceOf[Int])
            }

          case _ =>
        }
      }

      map.nodeStatus ++= statusChanges

      val msg = new ExplorationStatus(agent.currentAreaId, statusChanges.collect { case (node, status) => map.closeAreaIDs(agent.currentAreaId).byAreaId(map.toArea(node).getID) -> status }.toMap)
      println(msg)
      agent.sendSpeak(time, Constants.TO_STATION, Message.encode(msg))
    }
  }

}
