package mpmens

import scala.collection.mutable
import scala.reflect.ClassTag

trait RoleMembersMixin {
  this: Universe =>

  abstract class RoleMembers[+ComponentType <: Universe#Component](values: Iterable[ComponentType]) extends Members(values) {
    private[mpmens] def mapChildToParent(membersContainer: WithMembers[Universe#Component])
  }

  class RoleMembersStatic[+ComponentType <: Universe#Component](values: Iterable[ComponentType]) extends RoleMembers(values) {
    override def mapChildToParent(membersContainer: WithMembers[Universe#Component]): Unit = {
    }

    def select[RoleType <: Component : ClassTag]: RoleMembersStatic[RoleType] = {
      val comps = mutable.ListBuffer.empty[RoleType]

      for (value <- values) {
        value match {
          case comp: RoleType => comps += comp
          case _ =>
        }
      }

      new RoleMembersStatic[RoleType](comps)
    }

    def ++[B >: ComponentType <: Universe#Component](other: RoleMembersStatic[B]): RoleMembersStatic[B] =
      new RoleMembersStatic(values ++ other.values)

  }


  case class MirroredRoleMember[+ComponentType <: Universe#Component](value: ComponentType, parent: WithMembers[Universe#Component], indexInParent: Int)

  class RoleMembersImplied[+ComponentType <: Universe#Component](val mirroredRoleMembers: Iterable[MirroredRoleMember[ComponentType]]) extends RoleMembers(mirroredRoleMembers.map(_.value)) {

    /** Creates members from existing parent without any filtering. */
    def this(parent: WithMembers[ComponentType]) = this(parent.allMembers.values.zipWithIndex.map(memberAndIdx => MirroredRoleMember(memberAndIdx._1, parent, memberAndIdx._2)))

    override def mapChildToParent(membersContainer: WithMembers[Universe#Component]): Unit = {
      val members = mirroredRoleMembers.zipWithIndex
      for ((member, idx) <- members) {
        solverModel.ifThen(solverModel.member(idx, membersContainer.allMembersVar), solverModel.member(member.indexInParent, member.parent.allMembersVar))
      }
    }

    def selectImplied[RoleType <: Component : ClassTag]: RoleMembersImplied[RoleType] =
      new RoleMembersImplied(mirroredRoleMembers.collect{ case member@MirroredRoleMember(value: RoleType, parent, indexInParent) => MirroredRoleMember(value.asInstanceOf[RoleType], parent, indexInParent) })

    def ++[B >: ComponentType <: Universe#Component](other: RoleMembersImplied[B]): RoleMembersImplied[B] = {
      val members = mirroredRoleMembers ++ other.mirroredRoleMembers
      new RoleMembersImplied(members)
    }

  }

  class RoleMembersEquiv[+ComponentType <: Universe#Component](val mirroredRoleMembers: Iterable[MirroredRoleMember[ComponentType]]) extends RoleMembers(mirroredRoleMembers.map(_.value)) {

    /** Creates members from existing parent without any filtering. */
    def this(parent: WithMembers[ComponentType]) = this(parent.allMembers.values.zipWithIndex.map(memberAndIdx => MirroredRoleMember(memberAndIdx._1, parent, memberAndIdx._2)))

    override def mapChildToParent(membersContainer: WithMembers[Universe#Component]): Unit = {
      val members = mirroredRoleMembers.zipWithIndex
      for ((member, idx) <- members) {
        solverModel.ifOnlyIf(solverModel.member(idx, membersContainer.allMembersVar), solverModel.member(member.indexInParent, member.parent.allMembersVar))
      }
    }

    def selectEquiv[RoleType <: Component : ClassTag]: RoleMembersEquiv[RoleType] =
      new RoleMembersEquiv(mirroredRoleMembers.collect{ case member@MirroredRoleMember(value: RoleType, parent, indexInParent) => MirroredRoleMember(value.asInstanceOf[RoleType], parent, indexInParent) })

    def ++[B >: ComponentType <: Universe#Component](other: RoleMembersEquiv[B]): RoleMembersEquiv[B] = {
      val members = mirroredRoleMembers ++ other.mirroredRoleMembers
      new RoleMembersEquiv(members)
    }

  }

}
