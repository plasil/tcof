package mpmens

import mpmens.model.Component

import scala.collection.mutable

trait RolesMixin {
  this: System =>

  /** Represents a role in an ensemble. Implements methods to build membership over components contained in a role. */
  class Role[ComponentType <: Component](val name: String, private[mpmens] val allMembers: RoleMembers[ComponentType])
      extends SystemDelegates with WithMembers[ComponentType] {

    allMembers.mapChildToParent(this)

    override def toString(): String =
      s"""Role "$name"
          |${indent(selectedMembers.mkString("\n"), 1)}""".stripMargin
  }


  trait WithRoles {
    protected val roles = mutable.Map.empty[String, Role[_]]

    def role[ComponentType <: Component](name: String, items: RoleMembers[ComponentType]) = {
      val role = new Role[ComponentType](name, items)
      roles += (name -> role)
      role
    }

    def role[ComponentType <: Component](name: String) = roles(name).asInstanceOf[Role[ComponentType]]

  }

}
