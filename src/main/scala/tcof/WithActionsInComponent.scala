package tcof

trait WithActionsInComponent extends WithActions {
  this: Component =>

  private[tcof] override def _executePreActions(): Unit = _preActions.foreach(_())
  private[tcof] override def _executeActions(): Unit = _actions.foreach(_())
}
