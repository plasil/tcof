package mpmens
import scala.reflect.runtime.universe._

/** Represents a set of potential members (e.g. components that are to be used for a role, sub-ensembles of an ensembles).
  * In case the selection of members is dependent on the parent, it is specialized by MembersFromParent. In case the members
  * come from the universe (i.e. are not conditioned by existence in a parent), they are specialized as MembersFromUniverse.
  */
abstract class Members[MemberType](val values: Array[MemberType]) {
  def size = values.size

  def mapChildToParent(childRole: WithMembers[_]): Unit = {
  }

  def map[O](fun: MemberType => O): Array[O] = values map fun

  def withRole[RoleType <: MemberType : TypeTag]: Members[RoleType]
}
