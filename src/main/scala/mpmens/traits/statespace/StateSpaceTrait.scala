package mpmens.traits.statespace

import org.apache.commons.math3.analysis.interpolation.{LinearInterpolator, SplineInterpolator}
import org.apache.commons.math3.ode.FirstOrderDifferentialEquations
import org.apache.commons.math3.ode.events.EventHandler
import org.apache.commons.math3.ode.nonstiff.DormandPrince853Integrator

object interpolate {
  def linear(breakpoints: (Double, Double)*): Double => Double = {
    val fun = new LinearInterpolator().interpolate(breakpoints.map(_._1).toArray, breakpoints.map(_._2).toArray)
    (x: Double) => fun.value(x)
  }

  def spline(breakpoints: (Double, Double)*): Double => Double = {
    val fun = new SplineInterpolator().interpolate(breakpoints.map(_._1).toArray, breakpoints.map(_._2).toArray)
    (x: Double) => fun.value(x)
  }
}

abstract class StateSpaceModel private[statespace](val model: FirstOrderDifferentialEquations, val t0: Double, val y0: Array[Double]) {

  class StopEvent(val targetValue: Double, val idx: Int) extends EventHandler {
    override def init(t0: Double, y0: Array[Double], t: Double) = {}
    override def eventOccurred(t: Double, y: Array[Double], increasing: Boolean) = EventHandler.Action.STOP
    override def g(t: Double, y: Array[Double]) = y(idx) - targetValue
    override def resetState(t: Double, y: Array[Double]) = {}
  }

  protected def time(idx: Int, value: Double, limitTime: Double): Double = {
    val dp853 = new DormandPrince853Integrator(1.0e-8, 100.0, 1.0e-10, 1.0e-10)
    dp853.addEventHandler(new StopEvent(value, idx), 0.1, 1.0e-9, 1000)

    var y = new Array[Double](y0.size)
    dp853.integrate(model, t0, y0, limitTime, y)
  }

  protected def value(time: Double): Array[Double] = {
    val dp853 = new DormandPrince853Integrator(1.0e-8, 100.0, 1.0e-10, 1.0e-10)

    var y = new Array[Double](y0.size)
    dp853.integrate(model, t0, y0, time, y)
    y
  }
}

class StateSpaceModel1 private[statespace](model: FirstOrderDifferentialEquations, t0: Double, y0: Double) extends StateSpaceModel(model, t0, Array(y0)) {
  def valueAt(time: Double): Double = value(time)(0)
  def timeOf(value: Double, limitTime: Double) = time(0, value, limitTime)
}

class StateSpaceModelN private[statespace](model: FirstOrderDifferentialEquations, t0: Double, y0: Array[Double]) extends StateSpaceModel(model, t0, y0) {
  def valueAt(time: Double): Array[Double] = value(time)
  def timeOf(idx: Int, value: Double, limitTime: Double) = time(idx, value, limitTime)
}

trait StateSpaceTrait {

  def statespace(fun: Double => Double, t0: Double, y0: Double): StateSpaceModel1 = {
    val model = new FirstOrderDifferentialEquations {
      override def getDimension = 1
      override def computeDerivatives(t: Double, y: Array[Double], yDot: Array[Double]): Unit = {
        yDot(0) = fun(y(0))
      }
    }

    new StateSpaceModel1(model, t0, y0)
  }

  def statespace(fun: (Array[Double], Array[Double]) => Unit, t0: Double, y0: Array[Double]): StateSpaceModelN = {
    val dimensions = y0.size

    val model = new FirstOrderDifferentialEquations {
      override def getDimension = dimensions
      override def computeDerivatives(t: Double, y: Array[Double], yDot: Array[Double]): Unit = fun(y, yDot)
    }

    new StateSpaceModelN(model, t0, y0)
  }
}
