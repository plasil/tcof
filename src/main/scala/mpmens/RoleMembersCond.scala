package mpmens

import scala.reflect.ClassTag

/**
  * Collection of members that stems from another parent role. A member in this collection can be selected only if it is selected in the parent role.
  */
class RoleMembersCond[+ComponentType <: Component](val mirroredRoleMembers: Iterable[MirroredRoleMember[ComponentType]]) extends RoleMembers(mirroredRoleMembers.map(_.value)) {

  /** Creates members from existing parent without any filtering. */
  def this(parent: WithMembers[ComponentType]) = this(parent.allMembers.values.zipWithIndex.map{ case (member, idx) => MirroredRoleMember(member, parent, idx) })

  private[mpmens] override def mapChildToParent(membersContainer: WithMembers[Component]): Unit = {
    val members = mirroredRoleMembers.zipWithIndex
    for ((member, idx) <- members) {
      _solverModel.ifThen(_solverModel.member(idx, membersContainer.allMembersVar), _solverModel.member(member.indexInParent, member.parent.allMembersVar))
    }
  }

  def selectCond[RoleType <: Component : ClassTag]: RoleMembersCond[RoleType] =
    new RoleMembersCond(mirroredRoleMembers.collect{ case member@MirroredRoleMember(value: RoleType, parent, indexInParent) => MirroredRoleMember(value.asInstanceOf[RoleType], parent, indexInParent) })

  def ++[B >: ComponentType <: Component](other: RoleMembersCond[B]): RoleMembersCond[B] = {
    val members = mirroredRoleMembers ++ other.mirroredRoleMembers
    new RoleMembersCond(members)
  }

}
