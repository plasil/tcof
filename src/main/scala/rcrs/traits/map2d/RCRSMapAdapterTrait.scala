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


    case class CloseNodes(byIdx: Map[Int, map.Node], byNode: Map[map.Node, Int])
    val closeNodes = mutable.Map.empty[map.Node, CloseNodes]


    /** XXX - Not needed now. Remove if not needed in the near future.
      * Converts a consecutive path to a list of indexes of neighbors. This is meant as space conserving representation of a path
      * since the indexes fall in the range 0..15. Indexes are 1-based. Index 0 means the same node.
    def getPathAsNeighIdx(origin: map.Node, path: List[map.Node]) = {
      val originArea = toArea(origin)
      val areas = path.map(toArea)

      for {
        (from, to) <- (originArea :: areas).zip(areas)

        toIdx = if (from == to)
          0
        else {
          val neighIdx = from.getNeighbours.asScala.indexOf(to.getID)
          require(neighIdx >=0 )
          neighIdx + 1
        }
      } yield toIdx
    }

    def getPathFromNeighIdx(origin: map.Node, pathAsNeighIdx: List[Int]) = {
      val originArea = toArea(origin)

      pathAsNeighIdx.foldLeft((List.empty[map.Node], originArea))((pathAndArea, idx) => {
        val node = if (idx == 0) toNode(pathAndArea._2.getID) else toNode(pathAndArea._2.getNeighbours.get(idx - 1))
        (node :: pathAndArea._1, toArea(node))
      })._1.reverse
    }
    */


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

      for (node <- map.nodes) {
        val nodes = map.nodes.filter(n => node.center.distanceTo(n.center) < 100000).sortBy(n => node.center.distanceTo(n.center)).zipWithIndex
        closeNodes += node -> CloseNodes(nodes.map(_.swap).toMap, nodes.toMap)
      }

    }

    object RCRSAreaExploration {
      def apply(origin: map.Node, toExplore: Set[map.Node]): map.AreaExploration = map.AreaExploration(origin, toExplore, lineOfSight)
    }

  }

  implicit def map2dToRcrsMap2D(value: Map2D[RCRSNodeStatus]) = rcrsMap
  implicit def areaExplorationToRcrsAreaExploration(value: map.AreaExploration.type) = rcrsMap.RCRSAreaExploration
}
