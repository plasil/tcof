package rcrs.searching

import rescuecore2.worldmodel.EntityID
import scala.collection.mutable

class StationWorldInfo {
  var buildingsOnFire = new mutable.HashMap[EntityID, Integer]
}
