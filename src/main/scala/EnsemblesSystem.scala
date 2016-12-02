import org.chocosolver.solver.Model
import org.chocosolver.solver.constraints.nary.cnf.{ILogical, LogOp}
import org.chocosolver.solver.variables.{BoolVar, IntVar}

import scala.collection.mutable

class EnsemblesSystem {
  private val INT_MAX_VALUE = IntVar.MAX_INT_BOUND
  private val INT_MIN_VALUE = IntVar.MIN_INT_BOUND

  private val model = new Model()
  private var solving = false

  private val ensembles = mutable.ListBuffer.empty[Ensemble[_]]

  private var totalCostVar: IntVar = null

  private def indent(str: String, level: Int) = str.lines.map("  " * level + _).mkString("\n")

  abstract class Logical
  case class LogicalBoolean(val value: Boolean) extends Logical

  abstract class LogicalClause extends Logical
  case class LogicalLogOp(val value: LogOp) extends LogicalClause
  case class LogicalBoolVar(val value: BoolVar) extends LogicalClause
  case class LogicalEmpty() extends LogicalClause

  class Role[ComponentType <: Component](val name: String, val items: List[ComponentType]) {
    private val itemsArray = items.toArray[Component]

    private val membershipVar = model.setVar(name, Array.empty[Int], 0 until items.size toArray)

    class Cardinality {
      def is(num: Int) = LogicalBoolVar(model.arithm(membershipVar.getCard, "=", num).reify())
    }

    def cardinality: Cardinality = new Cardinality

    def sum(fun: ComponentType => Int): IntVar = {
      val sumVar = model.intVar(INT_MIN_VALUE, INT_MAX_VALUE)
      model.sumElements(membershipVar, items.map(fun).toArray, sumVar).post()
      sumVar
    }

    private def getImplies(fun: ComponentType => Logical, combinator: Seq[ILogical] => LogOp) = {
      val clauses = mutable.ListBuffer.empty[ILogical]

      for (idx <- 0 until itemsArray.size) {
        fun(items(idx)) match {
          case LogicalBoolean(value) => if (!value) clauses += model.notMember(idx, membershipVar).reify
          case LogicalBoolVar(value) => clauses += LogOp.implies(model.member(idx, membershipVar).reify, value)
          case LogicalLogOp(value) => clauses += LogOp.implies(model.member(idx, membershipVar).reify, value)
          case LogicalEmpty() =>
        }
      }

      if (clauses.size > 0)
        LogicalLogOp(combinator(clauses))
      else
        LogicalEmpty()
    }

    def all(fun: ComponentType => Logical): LogicalClause = getImplies(fun, LogOp.and(_ : _*))

    def some(fun: ComponentType => Logical): LogicalClause = getImplies(fun, LogOp.or(_ : _*))

    private def selectedItems = {
        import scala.collection.JavaConverters._
        (for (idx <- membershipVar.getValue.asScala) yield (idx.asInstanceOf[Int] -> items(idx))).toMap[Int, ComponentType]
    }

    override def toString(): String = s"""Role "$name":\n""" + indent(selectedItems.mkString("\n"), 1)
  }

  class Ensemble[AnchorType <: Component](val anchor: AnchorType) {
    private val roles = mutable.Map.empty[String, Role[_]]
    private[EnsemblesSystem] var costVar: IntVar = null

    def addRole[ComponentType <: Component](name: String, items: List[ComponentType]) = {
      val role = new Role[ComponentType](name, items)
      roles += (name -> role)
      role
    }

    def role[ComponentType <: Component](name: String) = roles(name).asInstanceOf[Role[ComponentType]]

    def cost(cst: IntVar) {
      costVar = cst
    }

    /* TODO, add variable that indicates if the ensemble is instantiated and condition the ensure clauses below and the cost by this variable */

    def ensure(clauses: LogicalClause*): Unit = {
      for (clause <- clauses) {
        clause match {
          case LogicalBoolVar(value) => model.addClauseTrue(value)
          case LogicalLogOp(value) => model.addClauses(value)
          case LogicalEmpty() =>
        }
      }
    }

    def cost: Int =
      if (costVar != null && costVar.isInstantiated)
        costVar.getValue
      else
        throw new UnsupportedOperationException("Ensemble does not have a cost assigned.")

    override def toString(): String = s"Ensemble:\n" + "  Anchor:\n" + indent(anchor.toString, 2) + "\n" + indent(roles.values.mkString("\n"), 1)
  }

  def addEnsemble[AnchorType <: Component](anchor: AnchorType): Ensemble[AnchorType] = {
    val ensemble = new Ensemble(anchor)
    ensembles += ensemble
    ensemble
  }

  def solve(): Boolean = {
    if (!solving) {
      val costVars = for (x <- ensembles if x.costVar != null) yield x.costVar
      if (costVars.size > 0) {
        totalCostVar = model.intVar(INT_MIN_VALUE, INT_MAX_VALUE)
        model.sum(costVars toArray, "=", totalCostVar).post()
        model.setObjective(Model.MINIMIZE, totalCostVar)
      }

      solving = true
    }

    model.getSolver().solve()
  }

  def totalCost: Int =
    if (totalCostVar != null && totalCostVar.isInstantiated)
      totalCostVar.getValue
    else
      throw new UnsupportedOperationException("System does not have a cost assigned.")

  override def toString(): String = ensembles.mkString("", "\n", "\n")

  object implicits {
    implicit def booleanToLogical(x: Boolean) = LogicalBoolean(x)
  }
}
