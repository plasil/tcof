package mpmens

import mpmens.Utils._

import scala.collection.mutable
import scala.reflect.ClassTag

trait RolesMixin {
  this: Universe =>

  /**
    * Collection of members (components) kept in a role
    */
  abstract class RoleMembers[+ComponentType <: Universe#Component](values: Iterable[ComponentType]) extends Members(values) {
    private[mpmens] def mapChildToParent(membersContainer: WithMembers[Universe#Component])
  }

  /**
    * Collection of members that stems from a collection.
    */
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

  /**
    * Collection of members that stems from another parent role. A member in this collection can be selected only if it is selected in the parent role.
    */
  class RoleMembersCond[+ComponentType <: Universe#Component](val mirroredRoleMembers: Iterable[MirroredRoleMember[ComponentType]]) extends RoleMembers(mirroredRoleMembers.map(_.value)) {

    /** Creates members from existing parent without any filtering. */
    def this(parent: WithMembers[ComponentType]) = this(parent.allMembers.values.zipWithIndex.map{ case (member, idx) => MirroredRoleMember(member, parent, idx) })

    override def mapChildToParent(membersContainer: WithMembers[Universe#Component]): Unit = {
      val members = mirroredRoleMembers.zipWithIndex
      for ((member, idx) <- members) {
        solverModel.ifThen(solverModel.member(idx, membersContainer.allMembersVar), solverModel.member(member.indexInParent, member.parent.allMembersVar))
      }
    }

    def selectCond[RoleType <: Component : ClassTag]: RoleMembersCond[RoleType] =
      new RoleMembersCond(mirroredRoleMembers.collect{ case member@MirroredRoleMember(value: RoleType, parent, indexInParent) => MirroredRoleMember(value.asInstanceOf[RoleType], parent, indexInParent) })

    def ++[B >: ComponentType <: Universe#Component](other: RoleMembersCond[B]): RoleMembersCond[B] = {
      val members = mirroredRoleMembers ++ other.mirroredRoleMembers
      new RoleMembersCond(members)
    }

  }


  /**
    * Collection of members that stems from another parent role. A member in this collection can be selected if and only if it is selected in the parent role.
    */
  class RoleMembersEquiv[+ComponentType <: Universe#Component](val mirroredRoleMembers: Iterable[MirroredRoleMember[ComponentType]]) extends RoleMembers(mirroredRoleMembers.map(_.value)) {

    /** Creates members from existing parent without any filtering. */
    def this(parent: WithMembers[ComponentType]) = this(parent.allMembers.values.zipWithIndex.map{ case (member, idx) => MirroredRoleMember(member, parent, idx) })

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


  /** Represents a role in an ensemble. Implements methods to build membership over components contained in a role. */
  class Role[+ComponentType <: Universe#Component](val name: String, private[mpmens] val allMembers: RoleMembers[ComponentType])
      extends SystemDelegates with WithMembers[ComponentType] with Initializable {

    def cloneEquiv = new RoleMembersEquiv(this)

    def cloneCond = new RoleMembersCond(this)

    override def toString: String =
      s"""Role "$name":\n${indent(selectedMembers.map(_ + "\n").mkString(""), 1)}"""

    override private[mpmens] def _init(stage: Int) = {
      super._init(stage)
      stage match {
        case 1 =>
          allMembers.mapChildToParent(this)
        case _ =>
      }
    }
  }


  trait WithRoles extends Initializable {
    private[mpmens] val _roles: mutable.Map[String, Role[Universe#Component]] = mutable.Map.empty[String, Role[Universe#Component]]

    def role[ComponentType <: Universe#Component](items: RoleMembers[ComponentType]): Role[ComponentType] = role(randomName, items)
    def role[ComponentType <: Universe#Component](name: String, items: RoleMembers[ComponentType]): Role[ComponentType] = {
      val role = new Role[ComponentType](name, items)
      _roles += name -> role
      role
    }

    def role[ComponentType <: Universe#Component](name: String): Role[ComponentType] = _roles(name).asInstanceOf[Role[ComponentType]]

    override private[mpmens] def _init(stage: Int) = {
      super._init(stage)
      _roles.values.foreach(_._init(stage))
    }
  }

}
