package mpmens

class State(val id: Int, val name: String) {
  private[mpmens] var parent: StateSet[_ <: State] = null
  private[mpmens] var indexInParent: Int = _

  override def toString: String = s"$name"
}


