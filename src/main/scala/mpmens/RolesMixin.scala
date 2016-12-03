package mpmens

import mpmens.model.Component

trait RolesMixin {
  this: System =>

  /** Represents a role in an ensemble. Implements methods to build membership over components contained in a role. */
  class Role[ComponentType <: Component](val name: String, private[mpmens] val items: Members[ComponentType])
      extends SystemDelegates with WithMembers[ComponentType] {

    private[mpmens] val allMembersVar = solverModel.setVar(name, Array.empty[Int], 0 until items.size toArray)

    override def allMembers = items.values

    items.mapChildToParent(this)

    private def selectedItems = {
      import scala.collection.JavaConverters._
      for (idx <- allMembersVar.getValue.asScala) yield items.values(idx)
    }

    override def toString(): String =
      s"""Role "$name"
         |${indent(selectedItems.mkString("\n"), 1)}
       """.stripMargin
  }
}
