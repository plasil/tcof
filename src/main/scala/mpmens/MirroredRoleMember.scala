package mpmens

case class MirroredRoleMember[+ComponentType <: Component](value: ComponentType, parent: WithMembers[Component], indexInParent: Int)
