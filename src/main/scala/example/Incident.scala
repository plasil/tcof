package example

import mpmens.concerns.map2d.{Position, PositionAware}
import mpmens.model.Component

case class Incident(var position: Position) extends Component with PositionAware
