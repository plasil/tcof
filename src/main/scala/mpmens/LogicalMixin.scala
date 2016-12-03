package mpmens

import org.chocosolver.solver.constraints.nary.cnf.{ILogical, LogOp}
import org.chocosolver.solver.variables.{BoolVar, IntVar, SetVar}

trait LogicalHelper {
}

trait LogicalMixin extends LogicalHelper {
 this: WithSolverModel =>

  /** Result of an expression that can be directly instantiated (i.e. does not have to be represented as a variable in the solver. */
  case class LogicalBoolean(value: Boolean) extends Logical {
    protected type ValueType = Boolean

    override def &&(other: Logical): Logical = other match {
      case _ if !value => this
      case other => other
    }

    override def ||(other: Logical): Logical = other match {
      case _ if value => this
      case other => other
    }

    override def unary_!(): Logical = LogicalBoolean(!value)

    override def post(): Unit = {
      if (!value) solverModel.falseConstraint().post()
    }
  }

  /** Common functionality for LogicalLogOp and LogicalBoolVar. */
  abstract class LogicalWithILogic extends Logical {
    protected type ValueType <: ILogical

    override def &&(other: Logical): Logical = other match {
      case other: LogicalBoolean if other.value => this
      case other: LogicalBoolean if !other.value => other
      case other: LogicalLogOp => LogicalLogOp(LogOp.and(this.value, other.value))
      case other: LogicalBoolVar => LogicalLogOp(LogOp.and(this.value, other.value))
    }

    override def ||(other: Logical): Logical = other match {
      case other: LogicalBoolean if !other.value => this
      case other: LogicalBoolean if other.value => other
      case other: LogicalLogOp => LogicalLogOp(LogOp.or(this.value, other.value))
      case other: LogicalBoolVar => LogicalLogOp(LogOp.or(this.value, other.value))
    }
  }

  /** And/Or tree of clauses. This is used to represent clauses about membership of a component. */
  case class LogicalLogOp(value: LogOp) extends LogicalWithILogic {
    protected type ValueType = LogOp

    override def unary_!(): Logical = LogicalLogOp(LogOp.nand(value))

    override def post(): Unit = {
      solverModel.addClauses(value)
    }
  }

  /** Boolean variable clause. This is used to represent reified constraints (e.g. cardinality). */
  case class LogicalBoolVar(value: BoolVar) extends LogicalWithILogic {
    protected type ValueType = BoolVar

    override def unary_!(): Logical = LogicalBoolVar(value.not)

    override def post(): Unit = {
      solverModel.addClauseTrue(value)
    }
  }
}
