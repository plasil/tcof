package mpmens

object InitStages extends Enumeration {
  type InitStages = Value

  val ConfigPropagation, VarsCreation, RulesCreation = Value
}
