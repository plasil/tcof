package mpmens

import org.chocosolver.solver.constraints.nary.cnf.LogOp
import org.chocosolver.solver.variables.SetVar

trait WithMembers[+MemberType] extends WithSystemDelegates {
  import universe.implicits._

  private[mpmens] def allMembers: Members[MemberType]
  private[mpmens] var allMembersVar: SetVar = solverModel.setVar(Array.empty[Int], 0 until allMembers.size toArray)

  class Cardinality {
    def ==(num: Int): LogicalBoolVar = LogicalBoolVar(solverModel.arithm(allMembersVar.getCard, "=", num).reify())
    def !=(num: Int): LogicalBoolVar = LogicalBoolVar(solverModel.arithm(allMembersVar.getCard, "!=", num).reify())
    def <(num: Int): LogicalBoolVar = LogicalBoolVar(solverModel.arithm(allMembersVar.getCard, "<", num).reify())
    def >(num: Int): LogicalBoolVar = LogicalBoolVar(solverModel.arithm(allMembersVar.getCard, ">", num).reify())
    def <=(num: Int): LogicalBoolVar = LogicalBoolVar(solverModel.arithm(allMembersVar.getCard, "<=", num).reify())
    def >=(num: Int): LogicalBoolVar = LogicalBoolVar(solverModel.arithm(allMembersVar.getCard, ">=", num).reify())
  }

  def cardinality: Cardinality = new Cardinality

  def contains(component: Component): Logical = some(_ == component)

  def sum(fun: MemberType => Integer): Integer = universe.IntegerUtils.sumBasedOnMembership(allMembersVar, allMembers.values.map(fun))

  def all(fun: MemberType => Logical): Logical =
    universe.LogicalUtils.forAllSelected(allMembers.values.map(fun), allMembersVar)

  def some(fun: MemberType => Logical): Logical =
    universe.LogicalUtils.existsSelected(allMembers.values.map(fun), allMembersVar)

  def selectedMembers: Iterable[MemberType] = {
    import scala.collection.JavaConverters._
    val values = allMembers.values.toIndexedSeq
    for (idx <- allMembersVar.getValue.asScala) yield values(idx)
  }
}

