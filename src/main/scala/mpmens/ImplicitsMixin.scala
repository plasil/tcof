package mpmens

import scala.reflect.ClassTag

trait ImplicitsMixin {
  this: System =>

  object implicits {
    implicit def booleanToLogical(x: Boolean) = LogicalBoolean(x)
    implicit def seqToMembers[ComponentType <: Component](components: Seq[ComponentType]) = new RoleMembersStatic(components)
    implicit def roleToMembers[ComponentType <: Component](role: Role[ComponentType]) = new RoleMembersFromParentRole(role)
    implicit def intToInteger(value: Int) = new IntegerInt(value)
    implicit def ensembleGroupToMembers[EnsembleType <: Ensemble[_]](group: EnsembleGroup[EnsembleType]) = group.allMembers
    implicit def seqToWithMembersSeq[MemberType](memberGroups: Seq[WithMembers[MemberType]]) = new WithMembersSeq(memberGroups)
  }
}
