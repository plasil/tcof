package mpmens

trait WithActionsInEnsemble extends WithActions {
  this: Ensemble =>

  private[mpmens] override def _executeActions(): Unit = {
    for (group <- _ensembleGroups.values) {
      group.selectedMembers.foreach(_._executeActions())
    }

    if (_actions.nonEmpty) _actions.get.apply()
  }
}
