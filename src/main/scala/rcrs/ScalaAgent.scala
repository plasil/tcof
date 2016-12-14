package rcrs

import rescuecore2.standard.components.StandardAgent
import rescuecore2.standard.entities.StandardEntity

abstract class ScalaAgent[T <: StandardEntity] extends StandardAgent[T] {
  type AgentEntityType = T
}
