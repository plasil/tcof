package rcrs

object AgentType extends Enumeration {
  type AgentType = Value

  val FIRE_BRIGADE = Value(0)
  val AMBULANCE_TEAM = Value(1)
  val POLICE_FORCE = Value(2)
  val CENTRAL = Value(3)
}
