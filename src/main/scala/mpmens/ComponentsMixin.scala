package mpmens

trait ComponentsMixin {
  this: Universe =>

  class Component(val name: String) extends WithUtility with WithActionsInComponent {
    override def toString: String =
      s"""Component "$name" (utility: $solutionUtility)"""
  }
}
