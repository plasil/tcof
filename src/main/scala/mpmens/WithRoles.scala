package mpmens

import mpmens.InitStages.InitStages
import mpmens.Utils._

import scala.collection.mutable

trait WithRoles extends Initializable {
  this: WithConfig =>

  private[mpmens] val _roles: mutable.Map[String, Role[Component]] = mutable.Map.empty[String, Role[Component]]

  def role[ComponentType <: Component](items: RoleMembers[ComponentType]): Role[ComponentType] = role(randomName, items)
  def role[ComponentType <: Component](name: String, items: RoleMembers[ComponentType]): Role[ComponentType] = {
    val role = new Role[ComponentType](name, items)
    _roles += name -> role
    role
  }

  override private[mpmens] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)
    _roles.values.foreach(_._init(stage, config))
  }
}
