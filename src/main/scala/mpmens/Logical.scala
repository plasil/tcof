package mpmens

import org.chocosolver.solver.constraints.nary.cnf.{ILogical, LogOp}
import org.chocosolver.solver.variables.BoolVar

/** Parent of clauses used in membership. */
abstract class Logical {
  protected type ValueType
  protected def value: ValueType

  def &&(other: Logical): Logical
  def ||(other: Logical): Logical
  def unary_!(): Logical
  def ->(other: Logical): Logical = !this || other
}

/** Result of an expression that can be directly instantiated (i.e. does not have to be represented as a variable in the solver. */
private[mpmens] case class LogicalBoolean(value: Boolean) extends Logical {
  protected type ValueType = Boolean

  override def &&(other: Logical): Logical = if (!value) this else other

  override def ||(other: Logical): Logical = if (value) this else other

  override def unary_!(): Logical = LogicalBoolean(!value)
}

/** Common functionality for LogicalLogOp and LogicalBoolVar. */
private[mpmens] abstract class LogicalWithILogic extends Logical {
  protected type ValueType <: ILogical

  override def &&(other: Logical): Logical = other match {
    case LogicalBoolean(true) => this
    case LogicalBoolean(false) => other
    case other: LogicalLogOp => LogicalLogOp(LogOp.and(this.value, other.value))
    case other: LogicalBoolVar => LogicalLogOp(LogOp.and(this.value, other.value))
  }

  override def ||(other: Logical): Logical = other match {
    case LogicalBoolean(false) => this
    case LogicalBoolean(true) => other
    case other: LogicalLogOp => LogicalLogOp(LogOp.or(this.value, other.value))
    case other: LogicalBoolVar => LogicalLogOp(LogOp.or(this.value, other.value))
  }
}

/** And/Or tree of clauses. This is used to represent clauses about membership of a component. */
private[mpmens] case class LogicalLogOp(value: LogOp) extends LogicalWithILogic {
  protected type ValueType = LogOp

  override def unary_!(): Logical = LogicalLogOp(LogOp.nand(value))
}

/** Boolean variable clause. This is used to represent reified constraints (e.g. cardinality). */
private[mpmens] case class LogicalBoolVar(value: BoolVar) extends LogicalWithILogic {
  protected type ValueType = BoolVar

  override def unary_!(): Logical = LogicalBoolVar(value.not)
}



