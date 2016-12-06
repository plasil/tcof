package mpmens

trait WithMembersUtilsMixin {
  this: Universe =>

  class WithMembersIterable[MemberType](memberGroups: Iterable[WithMembers[MemberType]]) {
    def allDisjoint: Logical = LogicalBoolVar(solverModel.allDisjoint(memberGroups.map(_.allMembersVar).toArray : _*).reify())
  }

}
