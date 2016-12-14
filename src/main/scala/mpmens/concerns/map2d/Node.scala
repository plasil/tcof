package mpmens.concerns.map2d

class Node private[map2d](val center: Position) {
  private[map2d] var _neighbors = Map.empty[Node, DirectedEdge]
  def neighbors: Map[Node, DirectedEdge] = _neighbors
}
