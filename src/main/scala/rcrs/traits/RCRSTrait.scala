package rcrs.traits

import tcof.traits.Trait
import rcrs.ScalaAgent
import rescuecore2.messages.Command
import rescuecore2.worldmodel.ChangeSet

trait RCRSTrait extends Trait {
  def agent: ScalaAgent

  def rcrsTraitStep(time: Int, changes: ChangeSet, heard: List[Command]): Unit = {
  }

}
