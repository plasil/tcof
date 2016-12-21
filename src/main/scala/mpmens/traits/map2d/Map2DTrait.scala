package mpmens.traits.map2d

import mpmens.traits.Trait


trait Map2DTrait extends Trait {

  val map: Map2D = new Map2D

  override def traitInit(): Unit = {
    super.traitInit()
  }
}
