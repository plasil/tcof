package mpmens

import org.chocosolver.solver.constraints.nary.cnf.{ILogical, LogOp}
import org.chocosolver.solver.variables.{BoolVar, SetVar}

import scala.collection.mutable

trait LogicalMixin {
  this: SolverMixin =>

  private[mpmens] object LogicalUtils {
    def and(clauses: Seq[Logical]): Logical = {
      if (clauses.exists(_ match {
        case LogicalBoolean(value) if (!value) => true
        case _ => false
      })) {
        LogicalBoolean(false)
      } else {
        val ilogs = for {
          clause <- clauses
          if (!clause.isInstanceOf[LogicalBoolean])
        } yield clause match {
          case LogicalLogOp(value) => value
          case LogicalBoolVar(value) => value
        }

        LogicalLogOp(LogOp.and(ilogs: _*))
      }
    }

    def post(clause: Logical): Unit = {
      clause match {
        case LogicalBoolean(value) if (!value) => solverModel.falseConstraint().post()
        case LogicalBoolVar(value) => solverModel.addClauseTrue(value)
        case LogicalLogOp(value) => solverModel.addClauses(value)
        case _ =>
      }
    }

    /** Creates clauses that express the fact the membership in membersVar implies corresponding Logical in membersClauses */
    def conditionMembership(membersClauses: Array[Logical], membersVar: SetVar, combinator: Seq[ILogical] => LogOp, emptyBehavior: Boolean) = {
      val clauses = mutable.ListBuffer.empty[ILogical]

      for (idx <- 0 until membersClauses.size) {
        membersClauses(idx) match {
          case LogicalBoolean(value) => if (!value) clauses += solverModel.notMember(idx, membersVar).reify
          case LogicalBoolVar(value) => clauses += LogOp.implies(solverModel.member(idx, membersVar).reify, value)
          case LogicalLogOp(value) => clauses += LogOp.implies(solverModel.member(idx, membersVar).reify, value)
          case _ =>
        }
      }

      if (clauses.size > 0)
        LogicalLogOp(combinator(clauses))
      else
        LogicalBoolean(emptyBehavior)
    }
  }
}
