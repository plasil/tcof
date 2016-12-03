package mpmens

import mpmens.model.Component
import scala.collection.mutable
import scala.reflect.runtime.universe._

trait MembersMixin {
  this: System =>

  /** Represents a set of potential members (e.g. components that are to be used for a role, sub-ensembles of an ensembles).
    * In case the selection of members is dependent on the parent, it is specialized by MembersFromParent. In case the items
    * come from the universe (i.e. are not conditioned by existence in a parent), they are specialized as MembersFromUniverse.
    */
  abstract class Members[ComponentType <: Component](val components: Array[ComponentType]) {
    def size = components.size

    def mapChildToParent(childRole: Role[_]): Unit = {
    }

    def map(fun: ComponentType => Ensemble[ComponentType]): Array[Ensemble[ComponentType]] = components map fun

    def withRole[RoleType <: ComponentType : TypeTag]: Members[RoleType]
  }

  class MembersFromUniverse[ComponentType <: Component](components: Array[ComponentType]) extends Members(components) {
    override def withRole[RoleType <: ComponentType : TypeTag]: Members[RoleType] = {
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

  class MembersFromParent[ComponentType <: Component](
      components: Array[ComponentType],
      val parentRole: Role[_],
      val parentIndices: Array[Int]) extends Members(components) {

    /** Creates items from existing role without any filtering. */
    def this(role: Role[ComponentType]) = this(role.items.components, role, 0 until role.items.size)

    override def mapChildToParent(childRole: Role[_]): Unit = {
      for (idx <- 0 until size) {
        solverModel.ifThen(solverModel.member(idx, childRole.allMembersVar), solverModel.member(parentIndices(idx), parentRole.allMembersVar))
      }
    }

    override def withRole[RoleType <: ComponentType : TypeTag]: Members[RoleType] = {
      val comps = mutable.ListBuffer.empty[RoleType]
      val idxs = mutable.ListBuffer.empty[Int]

      for (idx <- 0 until components.size) {
        components(idx) match {
          case comp: RoleType =>
            comps += comp
            idxs += idx
        }
      }

      new MembersFromParent[RoleType](comps toArray, parentRole, idxs toArray)
    }
  }


}
