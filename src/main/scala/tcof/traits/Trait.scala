package tcof.traits

import rescuecore2.messages.Command
import rescuecore2.worldmodel.ChangeSet

trait Trait {
  def init(): Unit = {
  }

  protected def traitStep(): Unit = {
  }

}
