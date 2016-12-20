package mpmens.concerns.map2d

import de.ummels.prioritymap.PriorityMap

import scala.collection.mutable

class Map2D {

  object ShortestPath {
    val cache = mutable.Map.empty[Node, (List[Node], Map[Node, Double], Map[Node, Node])]
  }

  class ShortestPath(val source: Node) {
    //, val stoppingCondition: ((Node, Double)) => Boolean = (_) => false) {
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

      ShortestPath.cache.getOrElseUpdate(source, go(PriorityMap(source -> 0), List.empty[Node], Map.empty[Node, Double], Map.empty[Node, Node]))
    }

    def distanceTo(target: Node): Option[Double] = distances.get(target)

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

  def getShortesPath(from: Node, to: Node) = new ShortestPath(from).pathTo(to)

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

  class AreaExploration(var assumePath: List[Node], val leftBottom: Position, val rightTop: Position, val nodesInView: Node => Iterable[Node] = List(_)) {
    private val exploreMaxCount = 3
    private val backtrackingMaxCount = 1000

    def compute(signature: String, currentNode: Node, previousNode: Node, toExplore: Set[Node], distanceLimit: Double, backtrackingLimit: Int): Option[(List[Node], Double)] = {
      var signatureVar = signature
      var toExploreVar = toExplore
      var currentNodeVar = currentNode
      var previousNodeVar = previousNode
      var distanceLimitVar = distanceLimit
      var straightPathReversed = List.empty[Node]
      var straightPathDistance = 0.0

      var result: Option[(List[Node], Double)] = null

      while (result == null) { // Result remains null as long as we go along a path without any alternatives

        val nodesSeen = nodesInView(currentNodeVar) // Removes nodes from toExplore that we can see from the currentNode
        toExploreVar = toExploreVar -- nodesSeen

        val exploreMaxCountWithBacktrackingLimit = if (backtrackingLimit == 0) 1 else exploreMaxCount

        if (toExploreVar.isEmpty) {
          println("+ " + signatureVar)
          straightPathReversed = currentNodeVar :: straightPathReversed

          result = Some((straightPathReversed.reverse, straightPathDistance))

        } else {
          // Select a few closes nodes that are to be explored and assemble an array of nodes we should go to
          // in order to get to the selected nodes to be explored
          val dij = new ShortestPath(currentNodeVar)

          val nodesByDistanceIter = dij.nodesByDistance.iterator
          var toExploreCount = 0

          var previousNodeIsIncluded = false

          val neighborsToExplore = new Array[Node](exploreMaxCountWithBacktrackingLimit)
          var neighborsToExploreLen = 0

          while (toExploreCount < exploreMaxCountWithBacktrackingLimit && nodesByDistanceIter.hasNext) {
            val node = nodesByDistanceIter.next

            if (toExploreVar.contains(node)) {
              val neighbor = dij.pathTo(node).get.head
              toExploreCount = toExploreCount + 1

              if (neighbor == previousNodeVar) {
                // We include previous node only if there is no other node to go to. This is an optimization that prevents going back if there is something along the path we have already chosen
                previousNodeIsIncluded = true

              } else if (!neighborsToExplore.contains(neighbor)) {
                neighborsToExplore(neighborsToExploreLen) = dij.pathTo(node).get.head
                neighborsToExploreLen = neighborsToExploreLen + 1
              }
            }
          }

          if (neighborsToExploreLen == 0) {
            neighborsToExplore(neighborsToExploreLen) = previousNodeVar
            neighborsToExploreLen = neighborsToExploreLen + 1
          }

          // Now neighborsToExplore contains a neighbors to go to in order to get to selected nodes to be explored
          if (neighborsToExploreLen == 1) {
            val node = neighborsToExplore(0)
            val distanceToNode = currentNodeVar.center.distanceTo(node.center)

            if (distanceToNode < distanceLimitVar) {
              straightPathDistance = straightPathDistance + distanceToNode
              straightPathReversed = currentNodeVar :: straightPathReversed
              signatureVar = signatureVar + "1"
              previousNodeVar = currentNodeVar
              currentNodeVar = node
              distanceLimitVar = distanceLimitVar - distanceToNode

            } else {
              result = None
            }
          } else {

            // There are more neighborsToExplore, thus we recursively explore all and select the shortest path
            var bestPathSoFar: List[Node] = null
            var bestTotalDistanceSoFar: Double = distanceLimitVar

            var signatureDigit = neighborsToExploreLen

            for (node <- neighborsToExplore if node != null) {
              val distanceToNode = currentNodeVar.center.distanceTo(node.center)

              if (distanceToNode < bestTotalDistanceSoFar) {
                compute(signatureVar + signatureDigit, node, currentNodeVar, toExploreVar, bestTotalDistanceSoFar - distanceToNode, backtrackingLimit / neighborsToExploreLen) match {
                  case Some((pathSoFar, distanceSoFar)) =>
                    bestPathSoFar = pathSoFar
                    bestTotalDistanceSoFar = distanceSoFar + distanceToNode
                  case None =>
                }
              }

              signatureDigit = signatureDigit - 1
            }

            if (bestPathSoFar != null) {
              straightPathReversed = currentNodeVar :: straightPathReversed
              result = Some((straightPathReversed.reverse ++ bestPathSoFar, straightPathDistance + bestTotalDistanceSoFar))

            } else {
              result = None
            }
          }
        }
      }

      return result
    }

    private var source = assumePath.last



    // We include to toExplore only those destinations which are reachable
    private val dij = new ShortestPath(source)
    private val toExplore = dij.distances.keySet.filter(node => node.center.x >= leftBottom.x && node.center.y >= leftBottom.y && node.center.x < rightTop.x && node.center.y <= rightTop.y)

    var explorationPath = if (toExplore.isEmpty)
        List()
      else {
        compute("", source, null, toExplore, Int.MaxValue, backtrackingMaxCount) match {
          case Some((path, length)) => path.tail
          case None => List()
        }
      }
  }


  /**
    * Approximates optimal path starting at source that ensures that all nodes within rectangle [leftBottom, rightTop] are seen (i.e.
    * an agent is within sight distance.
    * @param source Node to start the part at
    * @param leftBottom Bottom left coordinate of the rectangle
    * @param rightTop Top right coordinate of the rectangle
    */
  def getExplorePath(source: Node, leftBottom: Position, rightTop: Position, nodesInView: Node => Iterable[Node] = List(_)): List[Node] = {
    new AreaExploration(List(source), leftBottom, rightTop, nodesInView).explorationPath
  }
}
