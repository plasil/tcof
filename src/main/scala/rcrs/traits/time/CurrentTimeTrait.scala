package rcrs.traits.time

import rcrs.traits.RCRSTrait
import rescuecore2.messages.Command
import rescuecore2.worldmodel.ChangeSet


trait CurrentTimeTrait extends RCRSTrait {
  private var _time = 0
  def time = _time

  override def rcrsTraitStep(time: Int, changes: ChangeSet, heard: List[Command]): Unit = {
    _time = time

    super.rcrsTraitStep(time, changes, heard)
  }
}
