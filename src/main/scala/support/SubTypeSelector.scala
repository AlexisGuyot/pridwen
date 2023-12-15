package pridwen.support

import shapeless.{HList, HNil, ::, DepFn1}
import scala.annotation.implicitNotFound

// Finds the first element in HList L that is a sub-type of SuperType. Output type is the first matching element.
@implicitNotFound("Implicit not found: SubTypeSelector[${H}]. ${H} does not contain any element with supertype ${SuperType}.")
trait SubTypeSelector[H <: HList, SuperType] extends DepFn1[H] with Serializable { type Out <: SuperType }
trait LowerPrioritySubTypeSelector {
  type Aux[H <: HList, SuperType, SubType <: SuperType] = SubTypeSelector[H, SuperType] { type Out = SubType }

  protected def inhabit_Type[H <: HList, SuperType, SubType <: SuperType](
    f: H => SubType
  ): Aux[H, SuperType, SubType] 
    = new SubTypeSelector[H, SuperType] { 
        type Out = SubType 
        def apply(hlist: H) = f(hlist) 
  }

  implicit def head_is_not_a_subtype [
    SuperType, Head, Tail <: HList, 
    SubType <: SuperType
  ](
    implicit
    search_in_tail: SubTypeSelector.Aux[Tail, SuperType, SubType]
  ) = inhabit_Type[Head::Tail, SuperType, SubType](
    (hlist: Head::Tail) => search_in_tail(hlist.tail)
  )
}
object SubTypeSelector extends LowerPrioritySubTypeSelector {
  def apply[H <: HList, SuperType](implicit ok: SubTypeSelector[H, SuperType]): Aux[H, SuperType, ok.Out] = ok

  implicit def head_is_a_subtype[SuperType, Head <: SuperType, Tail <: HList] 
    = inhabit_Type[Head::Tail, SuperType, Head](
      (hlist: Head::Tail) => hlist.head
  )
}