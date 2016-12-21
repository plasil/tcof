package mpmens.concerns.map2d

import de.ummels.prioritymap.PriorityMap

import scala.collection.mutable

trait WithShortestPath {
  this: Map2D =>

  object ShortestPath {
    private val cache = mutable.Map.empty[Node, (List[Node], Map[Node, Double], Map[Node, Node])]
    private var epoch = 0

    def invalidateCache(): Unit = {
      synchronized {
        cache.clear()
        epoch = epoch + 1
      }
    }
  }

  class ShortestPath(val origin: Node) {
    //, val stoppingCondition: ((Node, Double)) => Boolean = (_) => false) {
    val (nodesByDistance, distances, predecessors) = compute(origin)

    // Adapted from https://github.com/ummels/dijkstra-in-scala/blob/master/src/main/scala/de/ummels/dijkstra/DijkstraPriority.scala
    // Original version - Copyright (c) 2015, Michael Ummels <michael@ummels.de>
    private def compute(origin: Node): (List[Node], Map[Node, Double], Map[Node, Node]) = {
      def go(active: PriorityMap[Node, Double], nodesByDistance: List[Node], distances: Map[Node, Double], predecessors: Map[Node, Node]):
      (List[Node], Map[Node, Double], Map[Node, Node]) =
        if (active.isEmpty)
          (nodesByDistance.reverse.tail, distances, predecessors)
        else {
          val (node, cost) = active.head
          val neighbours = (for {
            edge <- node.neighbors.values
            if !distances.contains(edge.to) && cost + edge.cost < active.getOrElse(edge.to, Double.MaxValue)
          } yield edge.to -> (cost + edge.cost)) toMap

          val preds = neighbours mapValues (_ => node)
          go(active.tail ++ neighbours, node :: nodesByDistance, distances + (node -> cost), predecessors ++ preds)
        }

      var result: (List[Node], Map[Node, Double], Map[Node, Node]) = null
      var epoch = 0

      synchronized {
        ShortestPath.cache.get(origin) match {
          case Some(x) => result = x
          case None =>
        }

        epoch = ShortestPath.epoch
      }

      if (result == null) {
        result = go(PriorityMap(origin -> 0), List.empty[Node], Map.empty[Node, Double], Map.empty[Node, Node])

        synchronized {
          if (ShortestPath.epoch == epoch)
            ShortestPath.cache += (origin -> result)
        }
      }

      result
    }

    def distanceTo(target: Node): Option[Double] = distances.get(target)

    def pathTo(target: Node) = {
      def go(current: Node, pathSoFar: List[Node] = List()): List[Node] = {
        predecessors.get(current) match {
          case None => pathSoFar
          case Some(node) => go(node, current :: pathSoFar)
        }
      }

      if (origin == target)
        Some(List())
      else if (predecessors.contains(target))
        Some(go(target))
      else
        None
    }
  }

}
