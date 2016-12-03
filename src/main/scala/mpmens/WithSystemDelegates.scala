package mpmens

import org.chocosolver.solver.Model

trait WithSystemDelegates {
  def system: System
  def solverModel: Model
}
