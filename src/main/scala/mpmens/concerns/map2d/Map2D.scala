package mpmens.concerns.map2d

import de.ummels.prioritymap.PriorityMap

class Map2D {

  class Dijkstra(val source: Node) {
    private val (distances, predecessors) = compute(source)

    // Adapted from https://github.com/ummels/dijkstra-in-scala/blob/master/src/main/scala/de/ummels/dijkstra/DijkstraPriority.scala
    // Original version - Copyright (c) 2015, Michael Ummels <michael@ummels.de>
    private def compute(source: Node): (Map[Node, Double], Map[Node, Node]) = {
      def go(active: PriorityMap[Node, Double], acc: Map[Node, Double], pred: Map[Node, Node]):
      (Map[Node, Double], Map[Node, Node]) =
        if (active.isEmpty) (acc, pred)
        else {
          val (node, cost) = active.head
          val neighbours = (for {
            edge <- node.neighbors.values if !acc.contains(edge.to) //&& cost + edge.length < active.getOrElse(edge.to, Int.MaxValue)
          } yield edge.to -> (cost + edge.length)) toMap
          val preds = neighbours mapValues (_ => node)
          go(active.tail ++ neighbours, acc + (node -> cost), pred ++ preds)
        }

      go(PriorityMap(source -> 0), Map.empty, Map.empty)
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
  private var _edges = List.empty[DirectedEdge]

  def nodes: List[Node] = _nodes
  def edges: List[DirectedEdge] = _edges

  def addNode(center: Position): Node = {
    val node = new Node(center)
    _nodes = _nodes :+ node
    node
  }

  def addDirectedEdge(from: Node, to: Node): DirectedEdge = from._neighbors.get(to) match {
    case Some(edge) => edge

    case None =>
      val edge = new DirectedEdge(from, to)
      _edges = _edges :+ edge
      from._neighbors = from._neighbors + (from -> edge)
      edge
  }

  def getPath(from: Node, to: Node) = new Dijkstra(from).pathTo(to)
}
