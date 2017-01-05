package mpmens

import mpmens.Utils._

import scala.collection.mutable

trait ComponentsMixin {
  this: Universe =>

  trait Component extends WithName with WithUtility with WithStateSets with WithActionsInComponent with Initializable {
    private[mpmens] val _constraintsClauseFuns = mutable.ListBuffer.empty[() => Logical]

    def constraints(clause: => Logical): Unit = {
      _constraintsClauseFuns += clause _
    }

    override private[mpmens] def _init(stage: Int) {
      super._init(stage)

      stage match {
        case 1 =>
          if (_constraintsClauseFuns.nonEmpty)
            LogicalUtils.post(LogicalUtils.and(_constraintsClauseFuns.map(_.apply())))
        case _ =>
      }
    }

    override def toString: String =
      s"""Component "$name" (utility: $solutionUtility)\n${indent(_stateSets.values.mkString(""), 1)}"""
  }
}
