package mpmens.concerns.map2d

class Node private[map2d](val center: Position) {
  private[map2d] var _neighbors = Map.empty[Node, Edge]
  def neighbors: Map[Node, Edge] = _neighbors
  override def toString = s"Node(center = $center)"
}
