package mpmens.concerns.map2d

import de.ummels.prioritymap.PriorityMap

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class Map2D {
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



  private object ShortestPath {
    val cache = mutable.Map.empty[Node, (List[Node], Map[Node, Double], Map[Node, Node])]
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
            if !distances.contains(edge.to) && cost + edge.length < active.getOrElse(edge.to, Double.MaxValue)
          } yield edge.to -> (cost + edge.length)) toMap

          val preds = neighbours mapValues (_ => node)
          go(active.tail ++ neighbours, node :: nodesByDistance, distances + (node -> cost), predecessors ++ preds)
        }

      ShortestPath.cache.getOrElseUpdate(origin, go(PriorityMap(origin -> 0), List.empty[Node], Map.empty[Node, Double], Map.empty[Node, Node]))
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


  /**
    * Approximates optimal path starting at origin that ensures that all nodes within rectangle [leftBottom, rightTop] are seen (i.e.
    * an agent is within sight distance.
    * @param leftBottom Bottom left coordinate of the rectangle
    * @param rightTop Top right coordinate of the rectangle
    * @param nodesInView A function that returns nodes that can be seen from a node
    */
  class AreaExploration(private val origin: Node, val leftBottom: Position, val rightTop: Position, val nodesInView: Node => Iterable[Node] = List(_)) {
    private val exploreMaxCount = 3
    private val backtrackingMaxCount = 10000

    private var assumePathWithOrigin: List[Node] = List(origin)

    private var walkedPathWithOrigin: List[Node] = List(origin)

    private var currentTask: ComputationTask = null

    private class ComputationTask() {
      val localAssumePathWithOrigin = assumePathWithOrigin
      val exploreOrigin = localAssumePathWithOrigin.last
      val dij = new ShortestPath(exploreOrigin)
      val toExplore = dij.nodesByDistance.filter(node => node.center.x >= leftBottom.x && node.center.y >= leftBottom.y && node.center.x < rightTop.x && node.center.y <= rightTop.y)

      // Do a quick computation to have something to return now
      var explorationPathWithOrigin: List[Node] = if (toExplore.isEmpty) localAssumePathWithOrigin else localAssumePathWithOrigin ++ dij.pathTo(toExplore.head).get

      var explorationPathLength: Double = _

      var isInterrupted = false

      val future = Future({
        val assumePathDistance = localAssumePathWithOrigin.zip(localAssumePathWithOrigin.tail).map { case (start, end) => start.center.distanceTo(end.center) }.sum

        val adjustedToExplore = mutable.Set.empty[Node] ++ toExplore
        for (node <- localAssumePathWithOrigin) {
          adjustedToExplore --= nodesInView(node)
        }

        explorationPathLength = Double.MaxValue

        doComputation(localAssumePathWithOrigin.reverse.tail, assumePathDistance, "", exploreOrigin, null, adjustedToExplore.toSet, backtrackingMaxCount)
      })

      def interrupt(): Unit = {
        isInterrupted = true
      }

      def doComputation(pathSoFarReversed: List[Node], distanceSoFar: Double, signature: String, currentNode: Node, previousNode: Node, toExplore: Set[Node], backtrackingLimit: Int): Unit = {
        var signatureVar = signature
        var toExploreVar = toExplore
        var currentNodeVar = currentNode
        var previousNodeVar = previousNode
        var distanceSoFarVar = distanceSoFar
        var pathSoFarReversedVar = pathSoFarReversed

        var straightPath = true

        while (!isInterrupted && straightPath) {
          // Repeat as long as we go along a path without any alternatives

          val nodesSeen = nodesInView(currentNodeVar) // Removes nodes from toExplore that we can see from the currentNode
          toExploreVar = toExploreVar -- nodesSeen

          val exploreMaxCountWithBacktrackingLimit = if (backtrackingLimit == 0) 1 else exploreMaxCount

          if (toExploreVar.isEmpty) {
            // println("+ " + signatureVar)
            pathSoFarReversedVar = currentNodeVar :: pathSoFarReversedVar

            val resultingPath = pathSoFarReversedVar.reverse
            if (resultingPath.startsWith(assumePathWithOrigin)) {
              explorationPathWithOrigin = resultingPath
              explorationPathLength = distanceSoFarVar
            }
            straightPath = false

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

              if (distanceToNode + distanceSoFarVar < explorationPathLength) {
                pathSoFarReversedVar = currentNodeVar :: pathSoFarReversedVar
                distanceSoFarVar = distanceSoFarVar + distanceToNode
                signatureVar = signatureVar + "1"
                previousNodeVar = currentNodeVar
                currentNodeVar = node

                straightPath = true

              } else {
                straightPath = false
              }

            } else {

              // There are more neighborsToExplore, thus we recursively explore all and select the shortest path

              var signatureDigit = neighborsToExploreLen

              for (node <- neighborsToExplore if node != null) {
                val distanceToNode = currentNodeVar.center.distanceTo(node.center)

                if (distanceToNode + distanceSoFarVar < explorationPathLength) {
                  doComputation(currentNodeVar :: pathSoFarReversedVar, distanceSoFarVar + distanceToNode, signatureVar + signatureDigit, node, currentNodeVar, toExploreVar, backtrackingLimit / neighborsToExploreLen)
                }

                signatureDigit = signatureDigit - 1
              }

              straightPath = false
            }
          }
        }
      }
    }

    def assume(path: List[Node]): Unit = {
      val pathWithOrigin = walkedPathWithOrigin ++ path
      require(pathWithOrigin.startsWith(assumePathWithOrigin))

      assumePathWithOrigin = pathWithOrigin
    }

    def walked(path: List[Node]): Unit = {
      val pathWithOrigin = walkedPathWithOrigin ++ path
      require(assumePathWithOrigin.startsWith(pathWithOrigin))

      walkedPathWithOrigin = pathWithOrigin
    }

    def explorationPath: List[Node] = {

      var result: List[Node] = null

      if (currentTask != null) {
        val currentExplorationPath = currentTask.explorationPathWithOrigin

        if (currentExplorationPath.startsWith(assumePathWithOrigin)) {
          result = currentExplorationPath.drop(walkedPathWithOrigin.size)
        }
      }

      if (result == null) {
        if (currentTask != null) {
          currentTask.interrupt()
        }

        currentTask = new ComputationTask

        result = currentTask.explorationPathWithOrigin.tail
      }

      result
    }

  }
}
