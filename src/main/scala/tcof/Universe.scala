package tcof

import rescuecore2.messages.Command
import rescuecore2.worldmodel.ChangeSet
import tcof.traits.Trait

abstract class Universe extends Trait {
  private var _time = 0
  def time = _time

  def step(time: Int): Unit = {
    _time = time
    traitStep()
  }

  private var _universe = Seq.empty[Component]
  def components_= (univ: Seq[Component]): Unit = _universe = univ
  def components: Seq[Component] = _universe

  protected def root[EnsembleType <: RootEnsemble](builder: => EnsembleType): RootEnsembleAnchor[EnsembleType] = {
    new RootEnsembleAnchor(builder _)
  }
}
