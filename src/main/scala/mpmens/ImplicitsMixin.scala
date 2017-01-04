package mpmens

import scala.language.implicitConversions

trait ImplicitsMixin {
  this: Universe =>

  implicit def booleanToLogical(x: Boolean): LogicalBoolean = LogicalBoolean(x)
  implicit def iterableToMembersStatic[ComponentType <: Universe#Component](components: Iterable[ComponentType]): RoleMembersStatic[ComponentType] = new RoleMembersStatic(components)
  implicit def roleToMembersFromParentRole[ComponentType <: Universe#Component](role: Role[ComponentType]): RoleMembersImplied[ComponentType] = new RoleMembersImplied(role)
  implicit def roleToMembersMirror[ComponentType <: Universe#Component](role: Role[ComponentType]): RoleMembersEquiv[ComponentType] = new RoleMembersEquiv(role)
  implicit def intToInteger(value: Int): IntegerInt = IntegerInt(value)
  implicit def ensembleGroupToMembers[EnsembleType <: Ensemble](group: EnsembleGroup[EnsembleType]): EnsembleGroupMembers[EnsembleType] = group.allMembers
  implicit def iterableToWithMembersIterable[MemberType](memberGroups: Iterable[WithMembers[MemberType]]): WithMembersIterable[MemberType] = new WithMembersIterable(memberGroups)
}
