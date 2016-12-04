package mpmens

trait WithUtility {
  private var _utility: Integer = null

  def utility_= (cst: Integer): Unit = _utility = cst

  def utility: Integer = _utility

  def solutionUtility: Int = if (_utility == null) 0 else _utility.solutionValue
}
