package mpmens

import mpmens.model.Component

trait ImplicitsMixin {
  this: System =>

  object implicits {
    implicit def booleanToLogical(x: Boolean) = LogicalBoolean(x)
    implicit def seqToMembers[ComponentType <: Component](components: Seq[ComponentType]) = new MembersFromUniverse(components toArray)
    implicit def roleToMembers[ComponentType <: Component](role: Role[ComponentType]) = new MembersFromParent(role)
    implicit def intToInteger(value: Int) = new IntegerInt(value)
  }
}
