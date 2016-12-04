package mpmens

import org.chocosolver.solver.variables.{IntVar, SetVar}

trait IntegerMixin {
  this: SolverMixin =>

  private[mpmens] object IntegerUtils {
    def sumBasedOnMembership(membersVar: SetVar, values: Seq[Integer]): IntegerIntVar = {
      IntegerIntVar(
        if (values.forall(_.isInstanceOf[Int]))
          sumIntsBasedOnMembership(membersVar, values)
        else
          sumGenericBasedOnMembership(membersVar, values)
      )
    }

    private def sumIntsBasedOnMembership(membersVar: SetVar, values: Seq[Integer]) = {
      val sumVar = newIntVar
      solverModel.sumElements(membersVar, values.map(_.asInstanceOf[IntegerInt].value) toArray, sumVar).post()
      sumVar
    }

    private def sumGenericBasedOnMembership(membersVar: SetVar, values: Seq[Integer]): IntVar = {
      val condCostVars = new Array[IntVar](values.size)

      var idx = 0
      for (value <- values) {
        val condCostVar = newIntVar
        val costVarContraint = value match {
          case IntegerInt(value) => solverModel.arithm(condCostVar, "=", value)
          case IntegerIntVar(value) => solverModel.arithm(condCostVar, "=", value)
        }

        solverModel.ifThenElse(solverModel.member(idx, membersVar), costVarContraint, solverModel.arithm(condCostVar, "=", 0))
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

    def solutionValue: Int = value

    def +(other: Integer): Integer = other match {
      case IntegerInt(otherValue) => IntegerInt(value + otherValue)
      case IntegerIntVar(otherValue) => addIntAndIntVar(value, otherValue)
    }
  }

  private[mpmens] case class IntegerIntVar(value: IntVar) extends Integer {
    protected type ValueType = IntVar

    def solutionValue: Int = if (value.isInstantiated) value.getValue else 0

    def +(other: Integer): Integer = other match {
      case IntegerInt(otherValue) => addIntAndIntVar(otherValue, value)
      case IntegerIntVar(otherValue) => addIntVarAndIntVar(value, otherValue)
    }
  }
}
