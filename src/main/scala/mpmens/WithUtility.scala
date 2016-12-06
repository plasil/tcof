package mpmens

trait WithUtility {
  private var _utility: Option[Integer] = None

  def utility_= (cst: Integer): Unit = {
    require(cst != null)
    _utility = Some(cst)
  }

  def utility: Option[Integer] = _utility

  def solutionUtility: Int = _utility match {
    case Some(value) => value.solutionValue
    case None => 0
  }
}
