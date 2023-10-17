package pridwen.support

import shapeless.{HList, HNil, ::, DepFn1}
import shapeless.labelled.{FieldType => Field, field}
import shapeless.ops.hlist.{Prepend}



// Transforms anything to a HList (handles nesting). Output type is the resulting HList.
trait ToHList[T] extends DepFn1[T] with Serializable { type Out <: HList }
trait LowPriorityToHList {
  type Aux[T, H <: HList] = ToHList[T] { type Out = H }

  protected def inhabit_Type[T, H <: HList](
    f: T => H
  ): Aux[T, H] 
    = new ToHList[T] { 
      type Out = H 
      def apply(t: T): Out = f(t) 
  }

  implicit def default[T] = inhabit_Type[T, T::HNil](
    (t: T) => t :: HNil
  )
}
object ToHList extends LowPriorityToHList {
  //def apply[T](implicit ok: ToHList[T]): Aux[T, ok.Out] = ok

  implicit def t_is_a_hlist [
    Head, Tail <: HList, 
    HHead <: HList, HTail <: HList, 
    H <: HList
  ](
    implicit
    to_hlist_head: ToHList.Aux[Head, HHead],
    to_hlist_tail: ToHList.Aux[Tail, HTail],
    concat: Prepend.Aux[HHead, HTail, H]
  ) = inhabit_Type[Head::Tail, H](
    (t: Head::Tail) => concat(to_hlist_head(t.head), to_hlist_tail(t.tail))
  )

  implicit def t_is_an_empty_hlist = inhabit_Type[HNil, HNil](
    (t: HNil) => HNil
  )

  implicit def t_is_a_nested_field [ 
    FName, FType, 
    HFType <: HList
  ](
    implicit
    f_is_nested: IsSubTypeOf[FType, Field[_,_] :: HList :: HNil],
    to_hlist_ftype: ToHList.Aux[FType, HFType]
  ) = inhabit_Type[Field[FName,FType], Field[FName, HFType]::HNil](
    (t: Field[FName,FType]) => field[FName](to_hlist_ftype(t)) :: HNil
  )
}