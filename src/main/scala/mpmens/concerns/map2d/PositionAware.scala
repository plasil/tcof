package mpmens.concerns.map2d

import mpmens.model.Role

trait PositionAware extends Role {
  var position: Position
}
