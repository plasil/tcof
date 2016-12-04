package mpmens

import org.chocosolver.solver.Model

trait WithSystemDelegates {
  val universe: Universe
  def solverModel: Model
}
