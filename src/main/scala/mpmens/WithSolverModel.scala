package mpmens

import org.chocosolver.solver.Model
import org.chocosolver.solver.variables.IntVar

/**
  * Created by bures on 03. 12. 2016.
  */
trait WithSolverModel {
  /** Upper bound for integer variables of the solver */
  private[mpmens] val IntMaxValue = IntVar.MAX_INT_BOUND
  /** Lower bound for integer variables of the solver */
  private[mpmens] val IntMinValue = IntVar.MIN_INT_BOUND

  def solverModel: Model
}
