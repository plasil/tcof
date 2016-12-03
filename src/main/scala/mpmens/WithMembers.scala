package mpmens

import org.chocosolver.solver.variables.{IntVar, SetVar}

trait WithMembers[MemberType] extends WithSolverModel {
  private[mpmens] def allMembersVar: SetVar

  private[mpmens] def allMembers: Array[MemberType]

  class Cardinality(val membershipVar: SetVar) {
    def ==(num: Int): LogicalBoolVar = LogicalBoolVar(solverModel.arithm(membershipVar.getCard, "=", num).reify())
  }

  def cardinality: Cardinality = new Cardinality(allMembersVar)

  def sum(fun: MemberType => Int): IntVar = {
    val sumVar = solverModel.intVar(IntMinValue, IntMaxValue)
    solverModel.sumElements(allMembersVar, allMembers.map(fun), sumVar).post()
    sumVar
  }
}
