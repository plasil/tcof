package mpmens.traits.map2d

class Map2D extends WithShortestPath with WithAreaExploration {
  class Node private[map2d](val center: Position) {
    private[map2d] var _neighbors = Map.empty[Node, Edge]
    def neighbors: Map[Node, Edge] = _neighbors

    private var _lastVisitTime = Int.MinValue
    def lastVisitTime = _lastVisitTime
    def lastVisitTime_=(time: Int) = _lastVisitTime = time

    override def toString() = s"Node(${_lastVisitTime})"
  }

  class Edge private[map2d](val from: Node, val to: Node, private var _cost: Double) {
    def cost = _cost
    def cost_=(value: Double) = {
      _cost = value
      ShortestPath.invalidateCache()
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

  def addDirectedEdge(from: Node, to: Node, cost: Double): Edge = from._neighbors.get(to) match {
    case Some(edge) => edge

    case None =>
      val edge = new Edge(from, to, cost)
      _edges = _edges :+ edge
      from._neighbors = from._neighbors + (to -> edge)
      edge
  }
}
