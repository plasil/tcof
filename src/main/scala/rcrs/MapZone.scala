package rcrs

import mpmens.traits.map2d.{Map2D, Position}


case class MapZone(map: Map2D[_], xIdx: Int, yIdx: Int, maxLastVisitTime: Int) {
  override def toString: String = s"MapZone($xIdx, $yIdx)"

  val toExplore: Set[map.Node] = map.nodes.filter(node =>
    node.center.x >= xIdx * MapZone.xTileSize &&
      node.center.y >= yIdx * MapZone.yTileSize &&
      node.center.x < (xIdx + 1) * MapZone.xTileSize &&
      node.center.y <= (yIdx + 1) * MapZone.yTileSize &&
      node.lastVisitTime < maxLastVisitTime
  ).toSet

  val leftBottom = new Position(xIdx * MapZone.xTileSize, yIdx * MapZone.yTileSize)
  val rightTop = new Position((xIdx + 1) * MapZone.xTileSize, (yIdx + 1) * MapZone.yTileSize)
  val center = new Position((xIdx + 0.5) * MapZone.xTileSize, (yIdx + 0.5) * MapZone.yTileSize)
}

object MapZone {
  val xTileSize = 250000
  val yTileSize = 200000
}