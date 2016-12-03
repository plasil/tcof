package mpmens

import mpmens.model.Component

trait RolesMixin {
  this: System =>

  /** Represents a role in an ensemble. Implements methods to build membership over components contained in a role. */
  class Role[ComponentType <: Component](val name: String, private[mpmens] val allMembers: Members[ComponentType])
      extends SystemDelegates with WithMembers[ComponentType] {

    allMembers.mapChildToParent(this)

    override def toString(): String =
      s"""Role "$name"
         |${indent(selectedMembers.mkString("\n"), 1)}
       """.stripMargin
  }
}
