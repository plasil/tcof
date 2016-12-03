package mpmens

import org.chocosolver.solver.constraints.nary.cnf.{ILogical, LogOp}
import org.chocosolver.solver.variables.{IntVar, SetVar}

import scala.collection.mutable

trait WithMembers[MemberType] extends WithSolverModel with IntegerHelper {
  this: System#
  private[mpmens] def allMembersVar: SetVar

  private[mpmens] var allMembers: Members[MemberType]

  class Cardinality {
    def ==(num: Int): LogicalBoolVar = LogicalBoolVar(solverModel.arithm(allMembersVar.getCard, "=", num).reify())
  }

  def cardinality: Cardinality = new Cardinality

  def sum(fun: MemberType => Integer): Integer = sumBasedOnMembership(allMembersVar, allMembers.values.map(fun))

  private def getImplies(fun: MemberType => Logical, combinator: Seq[ILogical] => LogOp, emptyBehavior: Boolean) = {
    val clauses = mutable.ListBuffer.empty[ILogical]

    for (idx <- 0 until allMembers.size) {
      fun(allMembers.values(idx)) match {
        case LogicalBoolean(value) => if (!value) clauses += solverModel.notMember(idx, allMembersVar).reify
        case LogicalBoolVar(value) => clauses += LogOp.implies(solverModel.member(idx, allMembersVar).reify, value)
        case LogicalLogOp(value) => clauses += LogOp.implies(solverModel.member(idx, allMembersVar).reify, value)
      }
    }

    if (clauses.size > 0)
      LogicalLogOp(combinator(clauses))
    else
      LogicalBoolean(emptyBehavior)
  }

  def all(fun: MemberType => Logical): Logical = getImplies(fun, LogOp.and(_ : _*), true)

  def some(fun: MemberType => Logical): Logical = getImplies(fun, LogOp.or(_ : _*), false)
}
