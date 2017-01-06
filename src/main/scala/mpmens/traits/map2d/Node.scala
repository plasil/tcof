package mpmens.traits.map2d

class Node[NodeStatusType] private[map2d](val map: Map2D[NodeStatusType], val center: Position) {
  private[map2d] var _neighbors = Map.empty[Node[NodeStatusType], Edge[NodeStatusType]]
  def neighbors: Map[Node[NodeStatusType], Edge[NodeStatusType]] = _neighbors

  var lastVisitTime = Int.MinValue

  var status: NodeStatusType = _

  override def toString() = s"Node(${lastVisitTime})"
}
