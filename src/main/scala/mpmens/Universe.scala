package mpmens


abstract class Universe extends EnsemblesMixin with ComponentsMixin with RootEnsembleMixin with RolesMixin with EnsembleGroupsMixin with StateSetsMixin with ActionsMixin {
  private var _universe = Seq.empty[Component]
  def components_= (univ: Seq[Component]): Unit = _universe = univ
  def components: Seq[Component] = _universe
}
