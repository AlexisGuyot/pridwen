package pridwen.models.aux

import shapeless.{HList, HNil, ::, Witness}
import shapeless.labelled.{FieldType => Field}
import shapeless.ops.hlist.{Selector => HSelector}
import shapeless.ops.record.{Selector => RSelector}

import pridwen.models.{Model}

trait SelectAtt[M, P <: HList] { type Out <: HList }
trait LowPrioritySelectAtt3 {
    type Aux[M, P <: HList, Out0 <: HList] = SelectAtt[M, P] { type Out = Out0 }
    protected def inhabit_Type[M, P <: HList, Out0 <: HList]: Aux[M, P, Out0] = new SelectAtt[M, P] { type Out = Out0 }

    implicit def m_is_hlist_p_is_field_name[M <: HList, K, V, TP <: HList, Out0 <: HList](
        implicit
        s: RSelector.Aux[M, K, V],
        a: SelectAtt.Aux[V, TP, Out0]
    ) = inhabit_Type[M, K::TP, Out0]
}
trait LowPrioritySelectAtt2 extends LowPrioritySelectAtt3 {
    // More priority than m_is_hlist_p_is_field_name
    implicit def eor_m_is_hlist_p_is_field_name[M <: HList, K, V, TP <: HList, Out0 <: HList](
        implicit
        s: RSelector.Aux[M, K, V],
    ) = inhabit_Type[M, K::HNil, Field[K,V]::HNil]
}
trait LowPrioritySelectAtt1 extends LowPrioritySelectAtt2 {
    // More priority than eor_m_is_hlist_p_is_field_name
    implicit def m_is_hlist_p_is_field[M <: HList, K, V, TP <: HList, Out0 <: HList](
        implicit
        s: RSelector.Aux[M, K, V],
        a: SelectAtt.Aux[V, TP, Out0]
    ) = inhabit_Type[M, Field[K,V]::TP, Out0]
}
object SelectAtt extends LowPrioritySelectAtt1 {
    def apply[M, P <: HList](implicit ok: SelectAtt[M, P]): Aux[M, P, ok.Out] = ok

    implicit def m_is_model[M <: Model, P <: HList, Out0 <: HList](
        implicit
        a: SelectAtt.Aux[M#Schema, P, Out0]
    ) = inhabit_Type[M, P, Out0]

    // More priority than m_is_hlist_p_is_field
    implicit def eor_m_is_hlist_p_is_field[M <: HList, K, V, Out0 <: HList](
        implicit
        s: RSelector.Aux[M, K, V],
    ) = inhabit_Type[M, Field[K,V]::HNil, Field[K,V]::HNil]

    implicit def m_is_hnil[M] = inhabit_Type[M, HNil, HNil]
}