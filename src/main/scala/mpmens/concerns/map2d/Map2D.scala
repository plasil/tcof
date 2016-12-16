package mpmens.concerns.map2d

import de.ummels.prioritymap.PriorityMap

import scala.collection.mutable
import scala.util.Random

class Map2D {

  object Dijkstra {
    val cache = mutable.Map.empty[Node, (List[Node], Map[Node, Double], Map[Node, Node])]
  }

  class Dijkstra(val source: Node) {//, val stoppingCondition: ((Node, Double)) => Boolean = (_) => false) {
    val (nodesByDistance, distances, predecessors) = compute(source)

    // Adapted from https://github.com/ummels/dijkstra-in-scala/blob/master/src/main/scala/de/ummels/dijkstra/DijkstraPriority.scala
    // Original version - Copyright (c) 2015, Michael Ummels <michael@ummels.de>
    private def compute(source: Node): (List[Node], Map[Node, Double], Map[Node, Node]) = {
      def go(active: PriorityMap[Node, Double], nodesByDistance: List[Node], distances: Map[Node, Double], predecessors: Map[Node, Node]):
      (List[Node], Map[Node, Double], Map[Node, Node]) =
        if (active.isEmpty)
          (nodesByDistance.reverse.tail, distances, predecessors)
        else {
          val (node, cost) = active.head
          val neighbours = (for {
            edge <- node.neighbors.values
            if !distances.contains(edge.to) && cost + edge.length < active.getOrElse(edge.to, Double.MaxValue)
          } yield edge.to -> (cost + edge.length)) toMap

          val preds = neighbours mapValues (_ => node)
          go(active.tail ++ neighbours, node :: nodesByDistance, distances + (node -> cost), predecessors ++ preds)
        }

      Dijkstra.cache.getOrElseUpdate(source, go(PriorityMap(source -> 0), List.empty[Node], Map.empty[Node, Double], Map.empty[Node, Node]))
    }

    def distanceTo(target: Node): Option[Double] =  distances.get(target)

    def pathTo(target: Node) = {
      def go(current: Node, pathSoFar: List[Node] = List()): List[Node] = {
        predecessors.get(current) match {
          case None => pathSoFar
          case Some(node) => go(node, current :: pathSoFar)
        }
      }

      if (source == target)
        Some(List())
      else if (predecessors.contains(target))
        Some(go(target))
      else
        None
    }
  }

  private var _nodes = List.empty[Node]
  private var _edges = List.empty[Edge]

  def nodes: List[Node] = _nodes
  def edges: List[Edge] = _edges

  def addNode(center: Position): Node = {
    val node = new Node(center)
    _nodes = _nodes :+ node
    node
  }

  def addDirectedEdge(from: Node, to: Node): Edge = from._neighbors.get(to) match {
    case Some(edge) => edge

    case None =>
      val edge = new Edge(from, to)
      _edges = _edges :+ edge
      from._neighbors = from._neighbors + (to -> edge)
      edge
  }

  def getPath(from: Node, to: Node) = new Dijkstra(from).pathTo(to)

  var accumulatedTimeInDijkstra = 0L

  /**
    * Approximates optimal path starting at source that ensures that all nodes within rectangle [leftBottom, rightTop] are seen (i.e.
    * an agent is within sight distance.
    * @param source Node to start the part at
    * @param leftBottom Bottom left coordinate of the rectangle
    * @param rightTop Top right coordinate of the rectangle
    */
  def getExplorePath(source: Node, leftBottom: Position, rightTop: Position, nodesInView: Node => Iterable[Node] = List(_)): List[Node] = {
    val exploreMaxCount = 3
    val backtrackingMaxCount = 5000
    val backtrackingProbability = 0.5

    def compute(signature: String, currentNode: Node, previousNode: Node, toExplore: Set[Node], distanceLimit: Double, backtrackingLimit: Int): Option[(List[Node], Double)] = {
      // Remove nodes from toExplore that we can see from the currentNode
      val nodesSeen = nodesInView(currentNode)
      val updatedToExplore = toExplore -- nodesSeen

      if (updatedToExplore.nonEmpty) {
        // Returns at most maxCount closest nodes from toExplore
        val dij = new Dijkstra(currentNode)
        val closestNodesToExplore = dij.nodesByDistance.filter(updatedToExplore.contains(_)).slice(0, exploreMaxCount)

        // Get neighbors that we should continue to in order to explore the closestNodesToExplore
        var neighborsToExplore = closestNodesToExplore.map(dij.pathTo(_).get.head).toSet

        // This is an optimization that prevents going back if there is something along the path we have already chosen
        if (neighborsToExplore.size > 1 && previousNode != null) {
          neighborsToExplore = neighborsToExplore - previousNode
        }

        val neighSize = neighborsToExplore.size
        if (neighSize > 1) {
          val keepIdx = Random.nextInt(neighSize)
          val neighWithIdx = neighborsToExplore.zipWithIndex
          if (backtrackingLimit == 0)
            neighborsToExplore = neighWithIdx.collect{ case (node, idx) if idx == keepIdx => node }
          else
            neighborsToExplore = neighWithIdx.collect{ case (node, idx) if idx == keepIdx || Random.nextDouble() < backtrackingProbability => node }
        }

        // Now we go through the neighborsToExplore and branch
        var bestPathSoFar: List[Node] = null
        var bestTotalDistanceSoFar: Double = distanceLimit

        var signatureDigit = neighborsToExplore.size

        for (node <- neighborsToExplore) {
          val distanceToNode = currentNode.center.distanceTo(node.center)

          if (distanceToNode < bestTotalDistanceSoFar) {
            compute(signature + signatureDigit, node, currentNode, updatedToExplore, bestTotalDistanceSoFar - distanceToNode, backtrackingLimit / neighSize) match {
              case Some((pathSoFar, distanceSoFar)) =>
                val totalDistanceSoFar = distanceSoFar + distanceToNode
                if (totalDistanceSoFar < bestTotalDistanceSoFar) {
                  bestPathSoFar = pathSoFar
                  bestTotalDistanceSoFar = totalDistanceSoFar
                }
              case None =>
            }
          }

          signatureDigit = signatureDigit - 1
        }

        if (bestPathSoFar != null)
          Some((currentNode :: bestPathSoFar, bestTotalDistanceSoFar))
        else
          None

      } else {
        println("+ " + signature)
        Some((List(currentNode), 0))
      }
    }

    val dij = new Dijkstra(source)
    val toExplore = dij.distances.keySet.filter(node => node.center.x >= leftBottom.x && node.center.y >= leftBottom.y && node.center.x < rightTop.x && node.center.y <= rightTop.y)

    if (toExplore.isEmpty)
      List()
    else {
      compute("", source, null, toExplore, Int.MaxValue, backtrackingMaxCount) match {
        case Some((path, length)) => path.tail
        case None => List()
      }
    }
  }
}
