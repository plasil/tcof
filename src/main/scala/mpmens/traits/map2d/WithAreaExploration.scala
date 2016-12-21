package mpmens.traits.map2d

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait WithAreaExploration {
  this: Map2D =>

  object AreaExploration {
    def apply(origin: Node, toExplore: Set[Node], nodesInView: Node => Iterable[Node]): AreaExploration =
      new AreaExploration(origin, toExplore, nodesInView)
  }

  /**
    * Approximates optimal path starting at origin that ensures that all nodes within rectangle [leftBottom, rightTop] are seen (i.e.
    * an agent is within sight distance.
    * @param nodesToExplore List of nodes to be explored
    * @param nodesInView A function that returns nodes that can be seen from a node
    */
  class AreaExploration(private val origin: Node, val nodesToExplore: Set[Node], val nodesInView: Node => Iterable[Node]) {
    private val exploreMaxCount = 3
    private val backtrackingMaxCount = 10000

    private var assumePathWithOrigin: List[Node] = List(origin)

    private var walkedPathWithOrigin: List[Node] = List(origin)

    private var currentTask: ComputationTask = null

    private class ComputationTask() {
      val localAssumePathWithOrigin = assumePathWithOrigin
      val exploreOrigin = localAssumePathWithOrigin.last
      val dij = new ShortestPath(exploreOrigin)

      val toExplore = dij.nodesByDistance.filter(nodesToExplore.contains(_))

      // Do a quick computation to have something to return now
      var explorationPathWithOrigin: List[Node] = if (toExplore.isEmpty) localAssumePathWithOrigin else localAssumePathWithOrigin ++ dij.pathTo(toExplore.head).get

      var explorationPathLength: Double = _

      var isInterrupted = false

      val future = Future({
        val assumePathDistance = localAssumePathWithOrigin.zip(localAssumePathWithOrigin.tail).map { case (start, end) => start.neighbors(end).cost }.sum

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
              val distanceToNode = currentNodeVar.neighbors(node).cost

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
                val distanceToNode = currentNodeVar.neighbors(node).cost

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
