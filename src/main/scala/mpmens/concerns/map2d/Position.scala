package mpmens.concerns.map2d

case class Position(x: Double, y: Double) {
  def distanceTo(other: Position) = math.sqrt(math.pow(x - other.x, 2) + math.pow(y - other.y, 2))
}
