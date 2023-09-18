package pridwen.support

import shapeless.{HList, HNil, ::, DepFn1}
import scala.annotation.implicitNotFound

// Finds the first element in HList L that is a sub-type of ST. Output type is the first detected element.
@implicitNotFound("Implicit not found: SubTypeSelector[${L}]. ${L} does not contain any element with supertype ${ST}.")
trait SubTypeSelector[L <: HList, ST] extends DepFn1[L] with Serializable { type Out <: ST }
trait LowerPrioritySubTypeSelector {
  type Aux[L <: HList, ST, Out0 <: ST] = SubTypeSelector[L, ST] { type Out = Out0 }
  protected def inhabit_Type[L <: HList, ST, Out0 <: ST](f: L => Out0): Aux[L, ST, Out0] = new SubTypeSelector[L, ST] { type Out = Out0 ; def apply(l: L) = f(l) }

  implicit def head_is_not_a_st [
    ST, H, T <: HList, 
    Out0 <: ST
  ](
    implicit
    s: SubTypeSelector.Aux[T, ST, Out0]
  ) = inhabit_Type[H::T, ST, Out0](l => s(l.tail))
}
object SubTypeSelector extends LowerPrioritySubTypeSelector {
  def apply[L <: HList, ST](implicit ok: SubTypeSelector[L, ST]): Aux[L, ST, ok.Out] = ok

  implicit def head_is_a_st[ST, H <: ST, T <: HList] = inhabit_Type[H::T, ST, H](l => l.head)
}