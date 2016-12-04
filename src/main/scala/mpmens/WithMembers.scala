package mpmens

import org.chocosolver.solver.constraints.nary.cnf.LogOp
import org.chocosolver.solver.variables.SetVar

trait WithMembers[MemberType] extends WithSystemDelegates {
  private[mpmens] def allMembers: Members[MemberType]
  private[mpmens] var allMembersVar: SetVar = solverModel.setVar(Array.empty[Int], 0 until allMembers.size toArray)

  class Cardinality {
    def ==(num: Int): LogicalBoolVar = LogicalBoolVar(solverModel.arithm(allMembersVar.getCard, "=", num).reify())
  }

  def cardinality: Cardinality = new Cardinality

  def sum(fun: MemberType => Integer): Integer = system.IntegerUtils.sumBasedOnMembership(allMembersVar, allMembers.values.map(fun))

  def all(fun: MemberType => Logical): Logical =
    system.LogicalUtils.conditionMembership(allMembers.values.map(fun), allMembersVar, LogOp.and(_ : _*), true)

  def some(fun: MemberType => Logical): Logical =
    system.LogicalUtils.conditionMembership(allMembers.values.map(fun), allMembersVar, LogOp.or(_ : _*), false)

  protected def selectedMembers = {
    import scala.collection.JavaConverters._
    for (idx <- allMembersVar.getValue.asScala) yield allMembers.values(idx)
  }
}

