package mpmens

import org.chocosolver.solver.variables.SetVar

trait WithMembers[+MemberType] extends WithSystemDelegates {

  private[mpmens] def allMembers: Members[MemberType]

  private[mpmens] val allMembersVar: SetVar = solverModel.setVar(Array.empty[Int], 0 until allMembers.size toArray)

  class Cardinality {
    def ==(num: Int): LogicalBoolVar = LogicalBoolVar(solverModel.arithm(allMembersVar.getCard, "=", num).reify())
    def !=(num: Int): LogicalBoolVar = LogicalBoolVar(solverModel.arithm(allMembersVar.getCard, "!=", num).reify())
    def <(num: Int): LogicalBoolVar = LogicalBoolVar(solverModel.arithm(allMembersVar.getCard, "<", num).reify())
    def >(num: Int): LogicalBoolVar = LogicalBoolVar(solverModel.arithm(allMembersVar.getCard, ">", num).reify())
    def <=(num: Int): LogicalBoolVar = LogicalBoolVar(solverModel.arithm(allMembersVar.getCard, "<=", num).reify())
    def >=(num: Int): LogicalBoolVar = LogicalBoolVar(solverModel.arithm(allMembersVar.getCard, ">=", num).reify())
  }

  def cardinality: Cardinality = new Cardinality

  def contains(member: Any): Logical = some((x) => LogicalBoolean(x == member))

  def sum(fun: MemberType => Integer): Integer = universe.IntegerUtils.sumBasedOnMembership(allMembersVar, allMembers.values.map(fun))

  def all(fun: MemberType => Logical): Logical =
    universe.LogicalUtils.forAllSelected(allMembers.values.map(fun), allMembersVar)

  def some(fun: MemberType => Logical): Logical =
    universe.LogicalUtils.existsSelected(allMembers.values.map(fun), allMembersVar)

  def foreachBySelection(forSelected: MemberType => Unit, forNotSelected: MemberType => Unit): Unit = {
    val selection = allMembersVar.getValue
    for ((member, idx) <- allMembers.values.zipWithIndex) {
      if (selection.contains(idx))
        forSelected(member)
      else
        forNotSelected(member)
    }
  }

  def membersWithSelectionIndicator: Iterable[(Boolean, MemberType)] = {
    val selection = allMembersVar.getValue
    allMembers.values.zipWithIndex.map(memberAndIndex => (selection.contains(memberAndIndex._2), memberAndIndex._1))
  }

  def selectedMembers: Iterable[MemberType] = {
    import scala.collection.JavaConverters._
    val values = allMembers.values.toIndexedSeq
    for (idx <- allMembersVar.getValue.asScala) yield values(idx)
  }
}

