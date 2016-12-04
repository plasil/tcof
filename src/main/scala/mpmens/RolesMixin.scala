package mpmens

import scala.collection.mutable

trait RolesMixin {
  this: Universe =>

  /** Represents a role in an ensemble. Implements methods to build membership over components contained in a role. */
  class Role[+ComponentType <: Component](val name: String, private[mpmens] val allMembers: RoleMembers[ComponentType])
      extends SystemDelegates with WithMembers[ComponentType] {

    allMembers.mapChildToParent(this)

    override def toString(): String =
      s"""Role "$name"\n${indent(selectedMembers.map(_ + "\n").mkString(""), 1)}"""
  }


  trait WithRoles {
    protected val roles = mutable.Map.empty[String, Role[Component]]

    def role[ComponentType <: Component](name: String, items: RoleMembers[ComponentType]) = {
      val role = new Role[ComponentType](name, items)
      roles += (name -> role)
      role
    }

    def role[ComponentType <: Component](name: String) = roles(name).asInstanceOf[Role[ComponentType]]

  }

}
