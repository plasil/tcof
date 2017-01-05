package mpmens

import org.chocosolver.solver.variables.BoolVar

/** Boolean variable clause. This is used to represent reified constraints (e.g. cardinality). */
private[mpmens] case class LogicalBoolVar(value: BoolVar) extends LogicalWithILogic {
  protected type ValueType = BoolVar

  override def unary_!(): Logical = LogicalBoolVar(value.not)
}
