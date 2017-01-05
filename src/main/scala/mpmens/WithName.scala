package mpmens

trait WithName {
  private [mpmens] var _name = Utils.randomName

  def name(nm: String): Unit = _name = nm
  def name: String = _name
}
