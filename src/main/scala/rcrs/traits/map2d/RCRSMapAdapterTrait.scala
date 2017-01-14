package rcrs.traits.map2d

import tcof.traits.map2d.{Map2D, Map2DTrait, Node, Position}
import rcrs.traits.RCRSTrait
import rescuecore2.standard.entities.Area
import rescuecore2.worldmodel.EntityID

import scala.collection.JavaConverters._
import scala.collection.mutable

trait RCRSMapAdapterTrait extends RCRSTrait {
  this: Map2DTrait[RCRSNodeStatus] =>

  val rcrsMap: RCRSMap2D = new RCRSMap2D

  override def init(): Unit = {
    super.init()

    rcrsMap.populate()
  }

  class RCRSMap2D {
    private val areaIdToNode = mutable.Map.empty[EntityID, Node[RCRSNodeStatus]]
    private val nodeToArea = mutable.Map.empty[Node[RCRSNodeStatus], Area]

    def toNode(areaId: EntityID): Node[RCRSNodeStatus] = areaIdToNode(areaId)
    def toArea(node: Node[RCRSNodeStatus]): Area = nodeToArea(node)

    def toAreaID(path: List[Node[RCRSNodeStatus]]): List[EntityID] = path.map(toArea(_).getID)

    def currentNode = areaIdToNode(agent.currentAreaId)

    val lineOfSight = mutable.Map.empty[Node[RCRSNodeStatus], Set[Node[RCRSNodeStatus]]]

    val nodeStatus = mutable.Map.empty[Node[RCRSNodeStatus], RCRSNodeStatus]

    val closeAreaIDs = RCRSMapStatic.closeAreaIDs

    def getWalkedPath(origin: Node[RCRSNodeStatus], path: List[Node[RCRSNodeStatus]], history: List[Position]): List[Node[RCRSNodeStatus]] = {
      val histAreas = history.map( pos =>
        agent.model.getObjectsInRange(pos.x.toInt, pos.y.toInt, 0).asScala
        .collectFirst{ case area: Area if area.getShape.contains(pos.x, pos.y) => area }.get
      )

      // Logger.info(s"Computing walked segment:\n  origin=${ toArea(origin)}\n  path=${path.map(toArea).toString}\n  history=${ histAreas }")

      val histIterator = histAreas.iterator

      var pathArea = toArea(origin)
      var remainingPath = path
      var remainingAreas = path.map(toArea)
      val walkedPath = mutable.ListBuffer.empty[Node[RCRSNodeStatus]]

      while (histIterator.hasNext) {
        var histArea = histIterator.next

        if (histArea == pathArea || remainingAreas.contains(histArea)) {
          while (histArea != pathArea) {
            val pathNode = remainingPath.head
            remainingPath = remainingPath.tail
            remainingAreas = remainingAreas.tail
            pathArea = toArea(pathNode)
            walkedPath += pathNode
          }
        }

        // If the condition above does not hold, we are slightly off the track. Either this gets corrected later in
        // the histAreas or it gets corrected in the next walk
      }

      walkedPath.toList
    }

    def populate(): Unit = {
      RCRSMapStatic.initialize(agent.config, agent.model)

      val model = agent.model.asScala

      for (entity <- model) {
        entity match {
          case area: Area =>
            val node = map.addNode(Position(area.getX, area.getY))
            areaIdToNode += (area.getID -> node)
            nodeToArea += (node -> area)
          case _ =>
        }
      }

      for (entity <- model) {
        entity match {
          case area: Area =>
            val areaNode = areaIdToNode(area.getID)
            val areaPosition = areaNode.center

            for (neighborId <- area.getNeighbours asScala) {
              val neighbor = agent.model.getEntity(neighborId)
              neighbor match {
                case neighborArea: Area =>
                  val neighborNode = areaIdToNode(neighborId)
                  val neighborPosition = neighborNode.center
                  map.addDirectedEdge(areaNode, neighborNode, areaPosition.distanceTo(neighborPosition))

                case _ =>
              }
            }

          case _ =>
        }
      }

      lineOfSight ++= RCRSMapStatic.lineOfSight.map { case (area, areasInSight) => ( toNode(area) -> areasInSight.map(toNode)) }
    }

    def rcrsAreaExploration(origin: Node[RCRSNodeStatus], toExplore: Set[Node[RCRSNodeStatus]]): map.AreaExploration = map.areaExploration(origin, toExplore, lineOfSight)
  }


  implicit def map2dToRcrsMap2D(value: Map2D[RCRSNodeStatus]) = rcrsMap
}
