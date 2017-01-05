package mpmens

trait WithActions {
  private[mpmens] var _actions: Option[() => Unit] = None

  def actions(act: => Unit): Unit = {
    _actions = Some(act _)
  }

  private[mpmens] def _executeActions()
}
