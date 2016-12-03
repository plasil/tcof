package mpmens

import mpmens.model.Component
import scala.collection.mutable
import scala.reflect.runtime.universe._

trait MembersMixin extends WithSolverModel {

  class MembersFromUniverse[MemberType](components: Array[MemberType]) extends Members(components) {
    override def withRole[RoleType <: MemberType : TypeTag]: Members[RoleType] = {
      val comps = mutable.ListBuffer.empty[RoleType]

      for (idx <- 0 until components.size) {
        components(idx) match {
          case comp: RoleType =>
            comps += comp
        }
      }

      new MembersFromUniverse[RoleType](comps toArray)
    }
  }

  class MembersFromParent[MemberType](
      components: Array[MemberType],
      val parent: WithMembers[_],
      val parentIndices: Array[Int]) extends Members(components) {

    /** Creates members from existing parent without any filtering. */
    def this(parent: WithMembers[MemberType]) = this(parent.allMembers.values, parent, 0 until parent.allMembers.size)

    override def mapChildToParent(childRole: WithMembers[_]): Unit = {
      for (idx <- 0 until size) {
        solverModel.ifThen(solverModel.member(idx, childRole.allMembersVar), solverModel.member(parentIndices(idx), parent.allMembersVar))
      }
    }

    override def withRole[RoleType <: MemberType : TypeTag]: Members[RoleType] = {
      val comps = mutable.ListBuffer.empty[RoleType]
      val idxs = mutable.ListBuffer.empty[Int]

      for (idx <- 0 until components.size) {
        components(idx) match {
          case comp: RoleType =>
            comps += comp
            idxs += idx
        }
      }

      new MembersFromParent[RoleType](comps toArray, parent, idxs toArray)
    }
  }

}
