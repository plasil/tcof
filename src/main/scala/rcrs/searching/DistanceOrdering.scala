package rcrs.searching

import rescuecore2.standard.entities.{StandardWorldModel, StandardEntity}

/**
  * An ordering that sorts entities by distance to a reference point.
  */
class DistanceOrdering(val reference: StandardEntity, world: StandardWorldModel) extends Ordering[StandardEntity] {
  override def compare(a: StandardEntity, b: StandardEntity): Int = {
    val d1: Int = world.getDistance(reference, a)
    val d2: Int = world.getDistance(reference, b)
    return d1 - d2
  }
}
