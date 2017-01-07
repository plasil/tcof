package mpmens

object InitStages extends Enumeration {
  type InitStages = Value

  val ExtraDeclarations, ConfigPropagation, VarsCreation, RulesCreation = Value
}
