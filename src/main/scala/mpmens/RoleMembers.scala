package mpmens

/**
  * Collection of members (components) kept in a role
  */
abstract class RoleMembers[+ComponentType <: Component](values: Iterable[ComponentType]) extends Members(values) with WithConfig {
  private[mpmens] def mapChildToParent(membersContainer: WithMembers[Component])
}
