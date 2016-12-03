package mpmens

abstract class Integer {
  protected type ValueType
  protected def value: ValueType

  def solutionValue: Int

  def +(other: Integer): Integer
}
