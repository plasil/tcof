package rcrs

import mpmens.concerns.map2d.{Map2D, Node, Position}
import rescuecore2.standard.entities.{Area, Edge, StandardEntity}
import rescuecore2.worldmodel.EntityID

import scala.collection.JavaConverters._
import scala.collection.mutable

trait WithMap2D extends {
  this: ScalaAgent =>

  protected object map extends Map2D {
    private val areaIdToNode = mutable.Map.empty[EntityID, Node]
    private val nodeToArea = mutable.Map.empty[Node, StandardEntity]

    def toNode(areaId: EntityID) = areaIdToNode(areaId)
    def toArea(node: Node) = nodeToArea(node)

    // https://en.wikipedia.org/wiki/Centroid#Centroid_of_polygon
    private def getCentroid(edges: Iterable[Edge]) = {
      var cxSum, cySum, aSum: Double = 0

      for (edge <- edges) {
        val start = edge.getStart
        val end = edge.getEnd

        cxSum = cxSum + (start.getX + end.getX) * (start.getX * end.getY - end.getX * start.getY)
        cySum = cySum + (start.getY + end.getY) * (start.getX * end.getY - end.getX * start.getY)
        aSum = aSum + (start.getX * end.getY - end.getX * start.getY)
      }

      val a = aSum / 2
      val cx = cxSum / (6 * a)
      val cy = cySum / (6 * a)

      Position(cx, cy)
    }

    def populate(): Unit = {
      for (entity <- model.asScala) {
        entity match {
          case area: Area =>
            val node = map.addNode(getCentroid(area.getEdges asScala))
            map.areaIdToNode += (area.getID -> node)
            map.nodeToArea += (node -> area)
          case _ =>
        }
      }

      for (entity <- model.asScala) {
        entity match {
          case area: Area =>
            for (neighborId <- area.getNeighbours asScala) {
              map.addDirectedEdge(map.areaIdToNode(area.getID), map.areaIdToNode(neighborId))
            }
          case _ =>
        }
      }
    }
  }
}
