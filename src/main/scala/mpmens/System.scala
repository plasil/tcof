package mpmens

import mpmens.model.Component
import org.chocosolver.solver.Model
import org.chocosolver.solver.constraints.nary.cnf.{ILogical, LogOp}
import org.chocosolver.solver.variables.{BoolVar, IntVar}

import scala.collection.mutable

class System extends ItemsMixin with ImplicitsMixin with RolesMixin with EnsemblesMixin {
  /** Upper bound for integer variables of the solver */
  private[mpmens] val IntMaxValue = IntVar.MAX_INT_BOUND
  /** Lower bound for integer variables of the solver */
  private[mpmens] val IntMinValue = IntVar.MIN_INT_BOUND

  /** Model used by the solver. */
  private[mpmens] val model = new Model()

  /** Indicates that model is fully initialized. */
  private var solving = false

  /** Solver variable representing the total utility of the system */
  private var totalUtilityVar: IntVar = null

  /** Internal method used in pretty-printing solving results */
  private[mpmens] def indent(str: String, level: Int) = str.lines.map("  " * level + _).mkString("\n")

  /** Common parent for LogicalBoolean and LogicalClause. It is used by methods all and some. */
  abstract class Logical
  /** Result of an expression that can be directly instantiated (i.e. does not have to be represented as a variable in the solver. */
  case class LogicalBoolean(val value: Boolean) extends Logical

  /** Parent of clauses used in membership. */
  abstract class LogicalClause extends Logical
  /** And/Or tree of clauses. This is used to represent clauses about membership of a component. */
  case class LogicalLogOp(val value: LogOp) extends LogicalClause
  /** Boolean variable clause. This is used to represent reified constraints (e.g. cardinality). */
  case class LogicalBoolVar(val value: BoolVar) extends LogicalClause
  /** Empty clause. */
  case class LogicalEmpty() extends LogicalClause


  /** A set of all potential ensembles */
  //private val ensembles = mutable.ListBuffer.empty[Ensemble[_]]

  class Ensembles[EnsembleType <: Ensemble[_]](val generator: () => Array[EnsembleType]) {
  }

  def ensembles[EnsembleType <: Ensemble[_]](generator: => Array[EnsembleType]): Ensembles[EnsembleType] = new Ensembles(generator)

/*
  def addEnsemble[AnchorType <: Component](anchor: AnchorType): Ensemble[AnchorType] = {
    val ensemble = new Ensemble(anchor)
    ensembles += ensemble
    ensemble
  }
*/


  def solve(): Boolean = {
    if (!solving) {
      val utilityVars = for (x <- ensembles if x.utilityVar != null) yield x.utilityVar
      if (utilityVars.size > 0) {
        totalUtilityVar = model.intVar(IntMinValue, IntMaxValue)
        model.sum(utilityVars toArray, "=", totalUtilityVar).post()
        model.setObjective(Model.MAXIMIZE, totalUtilityVar)
      }

      solving = true
    }

    model.getSolver().solve()
  }

  def totalUtility: Int =
    if (totalUtilityVar != null && totalUtilityVar.isInstantiated)
      totalUtilityVar.getValue
    else
      0

  override def toString(): String = ensembles.mkString("", "\n", "\n")
}
