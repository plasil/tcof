package mpmens
import scala.reflect.ClassTag

/** Represents a set of potential members (e.g. components that are to be used for a role, sub-ensembles of an ensembles).
  * In case the selection of members is dependent on the parent, it is specialized by MembersFromParent. In case the members
  * come from the universe (i.e. are not conditioned by existence in a parent), they are specialized as MembersFromUniverse.
  */
abstract class Members[MemberType](private[mpmens] val values: Array[MemberType]) {
  private[mpmens] def size = values.size

  private[mpmens] def mapChildToParent(childRole: WithMembers[_]): Unit = {
  }

  def map[O : ClassTag](fun: MemberType => O): Array[O] = values map fun

  def withRole[RoleType <: MemberType : ClassTag]: Members[RoleType]
}
