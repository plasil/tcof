package example

import mpmens.Component
import mpmens.concerns.map2d.{Position, PositionAware}

case class Incident(var position: Position) extends Component with PositionAware
