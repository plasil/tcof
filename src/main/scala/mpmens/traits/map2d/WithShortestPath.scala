package mpmens.traits.map2d

import de.ummels.prioritymap.PriorityMap

import scala.collection.mutable

trait WithShortestPath[NodeStatusType] {
  this: Map2D[NodeStatusType] =>

  object ShortestPath {
    private val cache = mutable.Map.empty[Node[NodeStatusType], (List[Node[NodeStatusType]], Map[Node[NodeStatusType], Double], Map[Node[NodeStatusType], Node[NodeStatusType]])]
    private var epoch = 0

    def invalidateCache(): Unit = {
      synchronized {
        cache.clear()
        epoch = epoch + 1
      }
    }

    def apply(source: Node[NodeStatusType]): ShortestPath = new ShortestPath(source)
  }

  class ShortestPath(val origin: Node[NodeStatusType]) {
    val (nodesByDistance, distances, predecessors) = compute(origin)

    // Adapted from https://github.com/ummels/dijkstra-in-scala/blob/master/src/main/scala/de/ummels/dijkstra/DijkstraPriority.scala
    // Original version - Copyright (c) 2015, Michael Ummels <michael@ummels.de>
    private def compute(origin: Node[NodeStatusType]): (List[Node[NodeStatusType]], Map[Node[NodeStatusType], Double], Map[Node[NodeStatusType], Node[NodeStatusType]]) = {
      def go(active: PriorityMap[Node[NodeStatusType], Double], nodesByDistance: List[Node[NodeStatusType]], distances: Map[Node[NodeStatusType], Double], predecessors: Map[Node[NodeStatusType], Node[NodeStatusType]]):
      (List[Node[NodeStatusType]], Map[Node[NodeStatusType], Double], Map[Node[NodeStatusType], Node[NodeStatusType]]) =
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

      var result: (List[Node[NodeStatusType]], Map[Node[NodeStatusType], Double], Map[Node[NodeStatusType], Node[NodeStatusType]]) = null
      var epoch = 0

      synchronized {
        ShortestPath.cache.get(origin) match {
          case Some(x) => result = x
          case None =>
        }

        epoch = ShortestPath.epoch
      }

      if (result == null) {
        result = go(PriorityMap(origin -> 0), List.empty[Node[NodeStatusType]], Map.empty[Node[NodeStatusType], Double], Map.empty[Node[NodeStatusType], Node[NodeStatusType]])

        synchronized {
          if (ShortestPath.epoch == epoch)
            ShortestPath.cache += (origin -> result)
        }
      }

      result
    }

    def distanceTo(target: Node[NodeStatusType]): Option[Double] = distances.get(target)

    def pathTo(target: Node[NodeStatusType]) = {
      def go(current: Node[NodeStatusType], pathSoFar: List[Node[NodeStatusType]] = List()): List[Node[NodeStatusType]] = {
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
