package pridwen.support

import shapeless.{HList, HNil, ::, LabelledGeneric, Lazy}
import shapeless.labelled.{FieldType => Field, field}

trait DeepGeneric[T] extends Serializable { 
    type Repr <: HList
    def to(t: T): Repr
    def from(hlist: Repr): T
}
trait LowPriorityDeepGeneric {
    type Aux[T, Repr0 <: HList] = DeepGeneric[T] { type Repr = Repr0 }

    protected def inhabit_Type[T, Repr0 <: HList](
        to0: T => Repr0
    )(
        from0: Repr0 => T
    ): Aux[T, Repr0] 
        = new DeepGeneric[T] {
            type Repr = Repr0
            def to(t: T) = to0(t)
            def from(hlist: Repr) = from0(hlist)
    }

    implicit def t_is_a_hlist_without_a_nested_head [
        Head, Tail <: HList, 
        Repr0 <: HList
    ](
        implicit
        generic: DeepGeneric.Aux[Tail, Repr0]
    ) = inhabit_Type[Head::Tail, Head::Repr0](
        (t: Head::Tail) => t.head :: generic.to(t.tail)
    )(
        (hlist: Head::Repr0) => hlist.head :: generic.from(hlist.tail)
    )
}
object DeepGeneric extends LowPriorityDeepGeneric {
    implicit def t_is_a_case_class [
        T, ReprT, 
        Repr0 <: HList
    ](
        implicit
        generic: LabelledGeneric.Aux[T, ReprT],
        deep_generic: DeepGeneric.Aux[ReprT, Repr0]
    ) = inhabit_Type[T, Repr0](
        (t: T) => deep_generic.to(generic.to(t))
    )(
        (hlist: Repr0) => generic.from(deep_generic.from(hlist))
    )

    implicit def t_is_a_hlist_with_a_nested_head [
        HeadName, Head, Tail <: HList, 
        ReprHead, DeepReprHead <: HList, DeepReprTail <: HList
    ](
        implicit
        generic_head: LabelledGeneric.Aux[Head, ReprHead],
        deep_generic_head: Lazy[DeepGeneric.Aux[ReprHead, DeepReprHead]],
        deep_generic_tail: DeepGeneric.Aux[Tail, DeepReprTail]
    ) = inhabit_Type[Field[HeadName,Head]::Tail, Field[HeadName,DeepReprHead]::DeepReprTail](
        (t: Field[HeadName,Head]::Tail) => field[HeadName](deep_generic_head.value.to(generic_head.to(t.head))) :: deep_generic_tail.to(t.tail)
    )(
        (hlist: Field[HeadName,DeepReprHead]::DeepReprTail) => field[HeadName](generic_head.from(deep_generic_head.value.from(hlist.head))) :: deep_generic_tail.from(hlist.tail)
    )

    implicit def t_is_an_empty_hlist = inhabit_Type[HNil, HNil](
        (t: HNil) => HNil
    )(
        (r: HNil) => HNil
    )
}