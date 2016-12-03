package mpmens

import mpmens.model.Component

import scala.reflect.ClassTag

trait ImplicitsMixin {
  this: System =>

  object implicits {
    implicit def booleanToLogical(x: Boolean) = LogicalBoolean(x)
    implicit def seqToMembers[ComponentType <: Component : ClassTag](components: Seq[ComponentType]) = new MembersFromUniverse(components toArray)
    implicit def roleToMembers[ComponentType <: Component : ClassTag](role: Role[ComponentType]) = new MembersFromParent(role)
    implicit def intToInteger(value: Int) = new IntegerInt(value)
  }
}
