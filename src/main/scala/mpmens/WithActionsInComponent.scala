package mpmens

trait WithActionsInComponent extends WithActions {
  this: Component =>

  private[mpmens] override def _executeActions(): Unit = {
    if (_actions.nonEmpty) _actions.get.apply()
  }
}
