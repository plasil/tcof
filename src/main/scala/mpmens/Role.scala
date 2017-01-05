package mpmens

import mpmens.InitStages.InitStages
import mpmens.Utils._

/** Represents a role in an ensemble. Implements methods to build membership over components contained in a role. */
class Role[+ComponentType <: Component](val name: String, private[mpmens] val allMembers: RoleMembers[ComponentType]) extends WithMembers[ComponentType] with Initializable {

  def cloneEquiv = new RoleMembersEquiv(this)

  def cloneCond = new RoleMembersCond(this)

  override def toString: String =
    s"""Role "$name":\n${indent(selectedMembers.map(_ + "\n").mkString(""), 1)}"""

  override private[mpmens] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)
    allMembers._init(stage, config)

    stage match {
      case InitStages.RulesCreation =>
        allMembers.mapChildToParent(this)
      case _ =>
    }
  }
}
