package rcrs.traits

import mpmens.traits.map2d.Map2DTrait
import rcrs.ScalaAgent
import rcrs.traits.map2d.{RCRSMapAdapterTrait, RCRSNodeStatus}

trait RCRSConnectorTrait extends RCRSTrait with RCRSMapAdapterTrait {
  this: Map2DTrait[RCRSNodeStatus] =>

  var agent: ScalaAgent = _
}
