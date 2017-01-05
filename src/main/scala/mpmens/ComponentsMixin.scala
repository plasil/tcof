package mpmens

import mpmens.InitStages.InitStages
import mpmens.Utils._

import scala.collection.mutable

trait ComponentsMixin {
  this: Universe =>

  trait Component extends WithName with WithUtility with WithStateSets with WithActionsInComponent with WithConfig with CommonImplicits with Initializable {
    private[mpmens] val _constraintsClauseFuns = mutable.ListBuffer.empty[() => Logical]

    def constraints(clause: => Logical): Unit = {
      _constraintsClauseFuns += clause _
    }

    override private[mpmens] def _init(stage: InitStages, config: Config): Unit = {
      super._init(stage, config)

      stage match {
        case InitStages.RulesCreation =>
          if (_constraintsClauseFuns.nonEmpty)
            _solverModel.post(_solverModel.and(_constraintsClauseFuns.map(_.apply())))
        case _ =>
      }
    }

    override def toString: String =
      s"""Component "$name" (utility: $solutionUtility)\n${indent(_stateSets.values.mkString(""), 1)}"""

  }
}
