package mpmens

import org.chocosolver.solver.variables.{IntVar, SetVar}

trait IntegerMixin {
  this: Universe =>

  private[mpmens] object IntegerUtils {
    def sum(values: Iterable[Integer]): Integer = {
      val constValue = values.collect{case x: IntegerInt => x}.foldLeft(0)(_ + _.value)
      val intVars = values.collect{case x: IntegerIntVar => x}.map(_.value)

      if (intVars.isEmpty) {
        IntegerInt(constValue)
      } else {
        val sumVar =
          if (intVars.size == 1) // Not sure whether this optimization has any real effect. However since I wrote it already, I keep it here.
            intVars.head
          else {
            val sumVar = newIntVar
            solverModel.sum(intVars toArray, "=", sumVar).post()
            sumVar
          }

        if (constValue == 0)
          IntegerIntVar(sumVar)
        else
          addIntAndIntVar(constValue, sumVar)
      }
    }

    def sumBasedOnMembership(membersVar: SetVar, values: Iterable[Integer]): IntegerIntVar = {
      IntegerIntVar(
        if (values.forall(_.isInstanceOf[IntegerInt]))
          sumIntsBasedOnMembership(membersVar, values)
        else
          sumGenericBasedOnMembership(membersVar, values)
      )
    }

    private def sumIntsBasedOnMembership(membersVar: SetVar, values: Iterable[Integer]) = {
      val sumVar = newIntVar
      solverModel.sumElements(membersVar, values.map(_.asInstanceOf[IntegerInt].value) toArray, sumVar).post()
      sumVar
    }

    private def sumGenericBasedOnMembership(membersVar: SetVar, values: Iterable[Integer]): IntVar = {
      val condCostVars = new Array[IntVar](values.size)

      var idx = 0
      for (value <- values) {
        val condCostVar = newIntVar
        val costVarConstraint = value match {
          case IntegerInt(intVal) => solverModel.arithm(condCostVar, "=", intVal)
          case IntegerIntVar(intVar) => solverModel.arithm(condCostVar, "=", intVar)
        }

        solverModel.ifThenElse(solverModel.member(idx, membersVar), costVarConstraint, solverModel.arithm(condCostVar, "=", 0))
        condCostVars(idx) = condCostVar

        idx = idx + 1
      }

      val sumVar = newIntVar
      solverModel.sum(condCostVars, "=", sumVar).post()

      sumVar
    }
  }

  private def addIntAndIntVar(left: Int, right: IntVar): IntegerIntVar = {
    val sumVar = newIntVar
    solverModel.arithm(sumVar, "-", right, "=", left).post()
    IntegerIntVar(sumVar)
  }

  private def addIntVarAndIntVar(left: IntVar, right: IntVar): IntegerIntVar = {
    val sum = newIntVar
    solverModel.arithm(left, "+", right, "=", sum).post()
    IntegerIntVar(sum)
  }

  private[mpmens] case class IntegerInt(value: Int) extends Integer {
    protected type ValueType = Int

    override def solutionValue: Int = value

    override def +(other: Integer): Integer = other match {
      case IntegerInt(otherValue) => IntegerInt(value + otherValue)
      case IntegerIntVar(otherValue) => addIntAndIntVar(value, otherValue)
    }

    private def revRelOp(num: Integer, revOp: String, revFun: (Int, Int) => Boolean) = {
      num match {
        case i: IntegerInt => LogicalBoolean(revFun(i.value, value))
        case iVar: IntegerIntVar => LogicalBoolVar(solverModel.arithm(iVar.value, revOp, value).reify())
      }
    }

    override def ==(num: Integer): Logical = revRelOp(num, "=", (x, y) => x == y)
    override def !=(num: Integer): Logical = revRelOp(num, "!=", (x, y) => x != y)
    override def <(num: Integer): Logical = revRelOp(num, ">", (x, y) => x > y)
    override def >(num: Integer): Logical = revRelOp(num, "<", (x, y) => x < y)
    override def <=(num: Integer): Logical = revRelOp(num, ">=", (x, y) => x >= y)
    override def >=(num: Integer): Logical = revRelOp(num, "<=", (x, y) => x <= y)
  }

  private[mpmens] case class IntegerIntVar(value: IntVar) extends Integer {
    protected type ValueType = IntVar

    override def solutionValue: Int = if (value.isInstantiated) value.getValue else 0

    override def +(other: Integer): Integer = other match {
      case IntegerInt(otherValue) => addIntAndIntVar(otherValue, value)
      case IntegerIntVar(otherValue) => addIntVarAndIntVar(value, otherValue)
    }

    private def relOp(num: Integer, op: String) = {
      num match {
        case i: IntegerInt => LogicalBoolVar(solverModel.arithm(value, op, i.value).reify())
        case iVar: IntegerIntVar => LogicalBoolVar(solverModel.arithm(value, op, iVar.value).reify())
      }
    }

    override def ==(num: Integer): Logical = relOp(num, "=")
    override def !=(num: Integer): Logical = relOp(num, "!=")
    override def <(num: Integer): Logical = relOp(num, "<")
    override def >(num: Integer): Logical = relOp(num, ">")
    override def <=(num: Integer): Logical = relOp(num, "<=")
    override def >=(num: Integer): Logical = relOp(num, ">=")

  }
}
