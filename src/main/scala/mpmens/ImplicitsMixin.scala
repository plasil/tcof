package mpmens

import mpmens.model.Component

trait ImplicitsMixin {
  this: System =>

  object implicits {
    implicit def booleanToLogical(x: Boolean) = LogicalBoolean(x)
    implicit def seqToItems[ComponentType <: Component](components: Seq[ComponentType]) = new MembersFromUniverse(components toArray)
    implicit def roleToItems[ComponentType <: Component](role: Role[ComponentType]) = new MembersFromParent(role)
  }
}
