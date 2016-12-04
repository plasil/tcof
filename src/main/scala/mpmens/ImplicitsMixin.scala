package mpmens

import scala.language.implicitConversions

trait ImplicitsMixin {
  this: Universe =>

  object implicits {
    implicit def booleanToLogical(x: Boolean): LogicalBoolean = LogicalBoolean(x)
    implicit def iterableToMembers[ComponentType <: Component](components: Iterable[ComponentType]): RoleMembersStatic[ComponentType] = new RoleMembersStatic(components)
    implicit def roleToMembers[ComponentType <: Component](role: Role[ComponentType]): RoleMembersFromParentRole[ComponentType] = new RoleMembersFromParentRole(role)
    implicit def intToInteger(value: Int): IntegerInt = IntegerInt(value)
    implicit def ensembleGroupToMembers[EnsembleType <: Ensemble](group: EnsembleGroup[EnsembleType]): EnsembleGroupMembers[EnsembleType] = group.allMembers
    implicit def iterableToWithMembersIterable[MemberType](memberGroups: Iterable[WithMembers[MemberType]]): WithMembersIterable[MemberType] = new WithMembersIterable(memberGroups)
  }
}
