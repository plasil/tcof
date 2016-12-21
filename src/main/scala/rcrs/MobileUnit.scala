package rcrs

import mpmens.Component
import mpmens.traits.map2d.{Position, PositionAware}


abstract class MobileUnit(var position: Position) extends Component with PositionAware
