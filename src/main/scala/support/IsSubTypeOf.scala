package pridwen.support

import shapeless.{HList, HNil, ::}

// Checks if type T is a subtype of one of the types contained in HLIst L. Output type is T's super type.
trait IsSubTypeOf[T, L <: HList] { type Out }
trait LowPriorityIsSubTypeOf {
  type Aux[T, L <: HList, Out0] = IsSubTypeOf[T, L] { type Out = Out0 }
  protected def inhabit_Type[T, L <: HList, Out0]: Aux[T, L, Out0] = new IsSubTypeOf[T, L] { type Out = Out0 }

  implicit def t_is_not_subtype_of_head [
    T, H, Q <: HList, 
    Out0
  ](
    implicit
    i: IsSubTypeOf.Aux[T, Q, Out0]
  ) = inhabit_Type[T, H::Q, Out0]
}
object IsSubTypeOf extends LowPriorityIsSubTypeOf {
  def apply[T, L <: HList](implicit ok: IsSubTypeOf[T, L]): Aux[T, L, ok.Out] = ok

  implicit def t_is_subtype_of_head [
    T, H >: T, Q <: HList
  ] = inhabit_Type[T, H::Q, H]
}