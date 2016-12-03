package mpmens

import org.chocosolver.solver.variables.{IntVar, SetVar}

trait IntegerHelper {
  private[mpmens] def sumBasedOnMembership(membersVar: SetVar, values: Array[Integer]): Integer
}

trait IntegerMixin extends IntegerHelper {
 this: WithSolverModel =>

  private[mpmens] def sumBasedOnMembership(membersVar: SetVar, values: Array[Integer]): IntegerIntVar = {
    IntegerIntVar(
      if (values.forall(_.isInstanceOf[Int]))
        sumIntsBasedOnMembership(membersVar, values)
      else
        sumGenericBasedOnMembership(membersVar, values)
    )
  }

  private def sumIntsBasedOnMembership(membersVar: SetVar, values: Array[Integer]) = {
    val sumVar = newIntVar
    solverModel.sumElements(membersVar, values.map(_.asInstanceOf[IntegerInt].value), sumVar).post()
    sumVar
  }

  private def sumGenericBasedOnMembership(membersVar: SetVar, values: Array[Integer]): IntVar = {
    val condCostVars = new Array[IntVar](values.size)

    for (idx <- 0 until values.size) {
      val condCostVar = newIntVar
      val costVarContraint = values(idx) match {
        case IntegerInt(value) => solverModel.arithm(condCostVar, "=", value)
        case IntegerIntVar(value) => solverModel.arithm(condCostVar, "=", value)
      }

      solverModel.ifThenElse(solverModel.member(idx, membersVar), costVarContraint, solverModel.arithm(condCostVar, "=", 0))
      condCostVars(idx) = condCostVar
    }

    val sumVar = newIntVar
    solverModel.sum(condCostVars, "=", sumVar).post()

    sumVar
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

  case class IntegerInt(value: Int) extends Integer {
    protected type ValueType = Integer

    def solutionValue: Int = value

    def +(other: Integer): Integer = other match {
      case IntegerInt(otherValue) => IntegerInt(value + otherValue)
      case IntegerIntVar(otherValue) => addIntAndIntVar(value, otherValue)
    }
  }

  case class IntegerIntVar(value: IntVar) extends Integer {
    protected type ValueType = IntVar

    def solutionValue: Int = if (value.isInstantiated) value.getValue else 0

    def +(other: Integer): Integer = other match {
      case IntegerInt(otherValue) => addIntAndIntVar(otherValue, value)
      case IntegerIntVar(otherValue) => addIntVarAndIntVar(value, otherValue)
    }
  }
}
