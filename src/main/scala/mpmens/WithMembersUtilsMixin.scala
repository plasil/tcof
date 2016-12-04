package mpmens

trait WithMembersUtilsMixin {
  this: Universe =>

  class WithMembersSeq[MemberType](memberGroups: Seq[WithMembers[MemberType]]) {
    def allDisjoint: Logical = LogicalBoolVar(solverModel.allDisjoint(memberGroups.map(_.allMembersVar) : _*).reify())
  }

}
