package rcrs

import mpmens.concerns.map2d.{Map2D, Node, Position}
import rescuecore2.standard.entities.{Area, Edge}
import rescuecore2.worldmodel.EntityID

import scala.collection.JavaConverters._
import scala.collection.mutable

trait WithMap2D {
  this: ScalaAgent[_] =>

  protected val areaToNode = mutable.Map.empty[EntityID, Node]
  protected val map = new Map2D

  // https://en.wikipedia.org/wiki/Centroid#Centroid_of_polygon
  private def getCentroid(edges: Iterable[Edge]) = {
    var cxSum, cySum, aSum: Double = 0

    for (edge <- edges) {
      val start = edge.getStart
      val end = edge.getEnd

      cxSum = cxSum + (start.getX + end.getX) * (start.getX * end.getY - end.getX * start.getY)
      cySum = cxSum + (start.getY + end.getY) * (start.getX * end.getY - end.getX * start.getY)
      aSum = aSum + (start.getX * end.getY - end.getX * start.getY)
    }

    val a = aSum / 2
    val cx = cxSum / (6 * a)
    val cy = cySum / (6 * a)

    Position(cx, cy)
  }

  protected def populateMap(): Unit = {
    for (entity <- model.asScala) {
      entity match {
        case area: Area =>
          val node = map.addNode(getCentroid(area.getEdges asScala))
          areaToNode += (area.getID -> node)
      }
    }

    for (entity <- model.asScala) {
      entity match {
        case area: Area =>
          for (neighborId <- area.getNeighbours asScala) {
            map.addDirectedEdge(areaToNode(area.getID), areaToNode(neighborId))
          }
      }
    }
  }
}
