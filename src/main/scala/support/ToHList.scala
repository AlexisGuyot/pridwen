package pridwen.support

import shapeless.{HList, HNil, ::, DepFn1}
import shapeless.labelled.{FieldType, field}
import shapeless.ops.hlist.{Prepend}

// Transforms anything to a HList (handles nested HList). Output type is the resulting HList.
trait ToHList[T] extends DepFn1[T] with Serializable { type Out <: HList }
trait LowPriorityToHList {
  type Aux[T, Out0 <: HList] = ToHList[T] { type Out = Out0 }
  protected def inhabit_Type[T, Out0 <: HList](f: T => Out0): Aux[T, Out0] = new ToHList[T] { type Out = Out0 ; def apply(t: T): Out = f(t) }

  implicit def default[T] = inhabit_Type[T, T::HNil](t => t :: HNil)
}
object ToHList extends LowPriorityToHList {
  def apply[T](implicit ok: ToHList[T]): Aux[T, ok.Out] = ok

  implicit def input_is_already_a_hlist [
    H, T <: HList, 
    Out0 <: HList, Out1 <: HList, 
    Out2 <: HList
  ](
    implicit
    t1: ToHList.Aux[H, Out0],
    t2: ToHList.Aux[T, Out1],
    p: Prepend.Aux[Out0, Out1, Out2]
  ) = inhabit_Type[H::T, Out2](l => p(t1(l.head), t2(l.tail)))

  implicit def input_is_already_an_empty_hlist = inhabit_Type[HNil, HNil](x => HNil)

  implicit def input_is_a_nested_field [ 
    K, V, 
    Out0 <: HList
  ](
    implicit
    i: IsSubTypeOf[V, FieldType[_,_] :: HList :: HNil],
    t: ToHList.Aux[V, Out0]
  ) = inhabit_Type[FieldType[K,V], FieldType[K, Out0]::HNil](f => field[K](t(f)) :: HNil)

  implicit def input_is_a_product [
    A, B, 
    Out0 <: HList, Out1 <: HList
  ](
    implicit
    t1: ToHList.Aux[A, Out0],
    t2: ToHList.Aux[B, Out1]
  ) = inhabit_Type[Product2[A,B], Product2[Out0, Out1]:: HNil](p => (t1(p._1), t2(p._2)) :: HNil)
}