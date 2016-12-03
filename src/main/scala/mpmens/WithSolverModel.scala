package mpmens

import org.chocosolver.solver.Model
import org.chocosolver.solver.variables.IntVar

trait WithSolverModel {
  /** Upper bound for integer variables of the solver */
  private[mpmens] val IntMaxValue = IntVar.MAX_INT_BOUND
  /** Lower bound for integer variables of the solver */
  private[mpmens] val IntMinValue = IntVar.MIN_INT_BOUND

  private[mpmens] def newIntVar = solverModel.intVar(IntMinValue, IntMaxValue)

  private[mpmens] def solverModel: Model
}
