package mpmens

import scala.collection.mutable

trait WithActions {
  private[mpmens] var _actions = mutable.ListBuffer.empty[() => Unit]
  private[mpmens] var _preActions = mutable.ListBuffer.empty[() => Unit]

  def actions(act: => Unit): Unit = _actions += act _

  def preActions(act: => Unit): Unit = _preActions += act _

  private[mpmens] def _executeActions()
  private[mpmens] def _executePreActions()
}
