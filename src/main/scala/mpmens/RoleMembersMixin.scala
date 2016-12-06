package mpmens

import scala.collection.mutable
import scala.reflect.ClassTag

trait RoleMembersMixin {
  this: Universe =>

  abstract class RoleMembers[+ComponentType <: Component](values: Iterable[ComponentType]) extends Members(values) {
    private[mpmens] def mapChildToParent(membersContainer: WithMembers[Component])

    def withRole[RoleType <: Component : ClassTag]: Members[RoleType]
  }

  class RoleMembersStatic[ComponentType <: Component](values: Iterable[ComponentType]) extends RoleMembers(values) {
    override def mapChildToParent(membersContainer: WithMembers[Component]): Unit = {
    }

    override def withRole[RoleType <: Component : ClassTag]: RoleMembers[RoleType] = {
      val comps = mutable.ListBuffer.empty[RoleType]

      for (value <- values) {
        value match {
          case comp: RoleType => comps += comp
          case _ =>
        }
      }

      new RoleMembersStatic[RoleType](comps)
    }
  }

  class RoleMembersFromParentRole[ComponentType <: Component](values: Iterable[ComponentType], private val parent: WithMembers[Component], private val indicesInParent: IndexedSeq[Int]) extends RoleMembers(values) {

    /** Creates members from existing parent without any filtering. */
    def this(parent: WithMembers[ComponentType]) = this(parent.allMembers.values, parent, 0 until parent.allMembers.size)

    override def mapChildToParent(membersContainer: WithMembers[Component]): Unit = {
      for (idx <- 0 until size) {
        solverModel.ifThen(solverModel.member(idx, membersContainer.allMembersVar), solverModel.member(indicesInParent(idx), parent.allMembersVar))
      }
    }

    override def withRole[RoleType <: Component : ClassTag]: RoleMembers[RoleType] = {
      val comps = mutable.ListBuffer.empty[RoleType]
      val idxs = mutable.ArrayBuffer.empty[Int]

      var idx = 0
      for (value <- values) {
        value match {
          case comp: RoleType =>
            comps += comp
            idxs += idx
          case _ =>
        }
      }

      new RoleMembersFromParentRole[RoleType](comps, parent, idxs)
    }
  }


}
