package rcrs.traits.map2d

import mpmens.traits.map2d.{Map2D, Map2DTrait, Position}
import rcrs.traits.RCRSTrait
import rescuecore2.standard.entities.Area
import rescuecore2.worldmodel.EntityID

import scala.collection.JavaConverters._
import scala.collection.mutable

trait RCRSMapAdapterTrait extends RCRSTrait {
  this: Map2DTrait[RCRSNodeStatus] =>

  val rcrsMap: RCRSMap2D = new RCRSMap2D

  override def traitInit(): Unit = {
    super.traitInit()

    rcrsMap.populate()
  }

  class RCRSMap2D {
    private val areaIdToNode = mutable.Map.empty[EntityID, map.Node]
    private val nodeToArea = mutable.Map.empty[map.Node, Area]

    def toNode(areaId: EntityID): map.Node = areaIdToNode(areaId)
    def toArea(node: map.Node): Area = nodeToArea(node)

    def toAreaID(path: List[map.Node]): List[EntityID] = path.map(toArea(_).getID)

    def currentNode = areaIdToNode(agent.currentAreaId)

    val lineOfSight = mutable.Map.empty[map.Node, Set[map.Node]]

    val nodeStatus = mutable.Map.empty[map.Node, RCRSNodeStatus]

    val closeAreaIDs = RCRSMapStatic.closeAreaIDs

    def getWalkedPath(origin: map.Node, path: List[map.Node], history: List[Position]): List[map.Node] = {
      val histAreas = history.map( pos =>
        agent.model.getObjectsInRange(pos.x.toInt, pos.y.toInt, 0).asScala
        .collectFirst{ case area: Area if area.getShape.contains(pos.x, pos.y) => area }.get
      )

      // Logger.info(s"Computing walked segment:\n  origin=${ toArea(origin)}\n  path=${path.map(toArea).toString}\n  history=${ histAreas }")

      val histIterator = histAreas.iterator

      var pathArea = toArea(origin)
      var remainingPath = path
      var remainingAreas = path.map(toArea)
      val walkedPath = mutable.ListBuffer.empty[map.Node]

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

    object RCRSAreaExploration {
      def apply(origin: map.Node, toExplore: Set[map.Node]): map.AreaExploration = map.AreaExploration(origin, toExplore, lineOfSight)
    }

  }

  implicit def map2dToRcrsMap2D(value: Map2D[RCRSNodeStatus]) = rcrsMap
  implicit def areaExplorationToRcrsAreaExploration(value: map.AreaExploration.type) = rcrsMap.RCRSAreaExploration
}
