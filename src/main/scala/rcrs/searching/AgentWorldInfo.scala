package rcrs.searching

import rescuecore2.worldmodel.EntityID

import scala.collection.mutable

class AgentWorldInfo {
  var sentBuildingsOnFire = new mutable.HashSet[EntityID]
}
