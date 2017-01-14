package tcof

import scala.collection.mutable

trait WithActions {
  private[tcof] var _actions = mutable.ListBuffer.empty[() => Unit]
  private[tcof] var _preActions = mutable.ListBuffer.empty[() => Unit]

  def actions(act: => Unit): Unit = _actions += act _

  def preActions(act: => Unit): Unit = _preActions += act _

  private[tcof] def _executeActions()
  private[tcof] def _executePreActions()
}
