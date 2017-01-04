package mpmens

trait ActionsMixin {
  this: Universe =>

  trait WithActions {
    private[mpmens] var _actions: Option[() => Unit] = None

    def actions(act: => Unit): Unit = {
      _actions = Some(act _)
    }

    def executeActions()
  }

  trait WithActionsInComponent extends WithActions {
    this: Component =>

    def executeActions(): Unit = {
      if (_actions.nonEmpty) _actions.get.apply()
    }
  }


  trait WithActionsInEnsemble extends WithActions {
    this: Ensemble =>

    def executeActions(): Unit = {
      for (group <- ensembleGroups.values) {
        group.selectedMembers.foreach(_.executeActions())
      }

      if (_actions.nonEmpty) _actions.get.apply()
    }
  }

}

