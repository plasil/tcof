package mpmens

import mpmens.model.Component
import scala.collection.mutable
import scala.reflect.runtime.universe._

trait ItemsMixin {
  this: System =>

  /** Represents a set of components that are to be used for a role. If case the selection of the components is dependent
    * on the parent role, it is specialized by ItemsFromRole. In case the items come from the universe (i.e. are not
    * conditioned by existence in parent role), they are specialized as ItemsFromUniverse.
    */
  abstract class Items[ComponentType <: Component](val components: Array[ComponentType]) {
    def size = components.size

    def mapChildToParent(childRole: Role[_]): Unit = {
    }

    def map(fun: ComponentType => Ensemble[ComponentType]): Array[Ensemble[ComponentType]] = components map fun

    def withRole[RoleType <: ComponentType : TypeTag]: Items[RoleType]
  }

  class ItemsFromUniverse[ComponentType <: Component](components: Array[ComponentType]) extends Items(components) {
    override def withRole[RoleType <: ComponentType : TypeTag]: Items[RoleType] = {
      val comps = mutable.ListBuffer.empty[RoleType]

      for (idx <- 0 until components.size) {
        components(idx) match {
          case comp: RoleType =>
            comps += comp
        }
      }

      new ItemsFromUniverse[RoleType](comps toArray)
    }
  }

  class ItemsFromRole[ComponentType <: Component](
      components: Array[ComponentType],
      val parentRole: Role[_],
      val parentIndices: Array[Int]) extends Items(components) {

    /** Creates items from existing role without any filtering. */
    def this(role: Role[ComponentType]) = this(role.items.components, role, 0 until role.items.size)

    override def mapChildToParent(childRole: Role[_]): Unit = {
      for (idx <- 0 until size) {
        model.ifThen(model.member(idx, childRole.membershipVar), model.member(parentIndices(idx), parentRole.membershipVar))
      }
    }

    override def withRole[RoleType <: ComponentType : TypeTag]: Items[RoleType] = {
      val comps = mutable.ListBuffer.empty[RoleType]
      val idxs = mutable.ListBuffer.empty[Int]

      for (idx <- 0 until components.size) {
        components(idx) match {
          case comp: RoleType =>
            comps += comp
            idxs += idx
        }
      }

      new ItemsFromRole[RoleType](comps toArray, parentRole, idxs toArray)
    }
  }


}
