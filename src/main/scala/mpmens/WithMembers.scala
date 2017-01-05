package mpmens

import org.chocosolver.solver.variables.SetVar

trait WithMembers[+MemberType] extends WithSystemDelegates with Initializable {

  private[mpmens] def allMembers: Members[MemberType]

  private[mpmens] var allMembersVar: SetVar = null

  override private[mpmens] def _init(stage: Int) = {
    super._init(stage)
    stage match {
      case 0 =>
        allMembersVar = solverModel.setVar(Array.empty[Int], 0 until allMembers.size toArray)
      case _ =>
    }
  }

  def cardinality: Integer = new universe.IntegerIntVar(allMembersVar.getCard)

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
    allMembers.values.zipWithIndex.map{case (member, idx) => (selection.contains(idx), member)}
  }

  def selectedMembers: Iterable[MemberType] = {
    import scala.collection.JavaConverters._
    val values = allMembers.values.toIndexedSeq
    for (idx <- allMembersVar.getValue.asScala) yield values(idx)
  }
}

