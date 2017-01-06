package mpmens

trait WithActionsInComponent extends WithActions {
  this: Component =>

  private[mpmens] override def _executePreActions(): Unit = _preActions.foreach(_())
  private[mpmens] override def _executeActions(): Unit = _actions.foreach(_())
}
