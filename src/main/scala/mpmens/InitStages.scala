package mpmens

object InitStages extends Enumeration {
  type InitStages = Value

  val ConfigPropagation, VariableCreation, RulesCreation = Value
}
