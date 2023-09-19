package pridwen.models.aux

import shapeless.{HList, ::, HNil, Lazy}
import shapeless.labelled.{FieldType => Field, field}
import shapeless.ops.record.{Selector => RSelector}

trait SubSchema[S <: HList, SS <: HList] { type Out <: HList ; def apply(s: S): Out }
trait LowerPrioritySubSchema {
    type Aux[S <: HList, SS <: HList, Out0 <: HList] = SubSchema[S, SS] { type Out = Out0 }
    protected def inhabit_Type[S <: HList, SS <: HList, Out0 <: HList](f: S => Out0): SubSchema[S, SS] = new SubSchema[S, SS] { type Out = Out0 ; def apply(s: S) = f(s) }

    implicit def subschema_head_is_key[S <: HList, H, T <: HList, V, Out0 <: HList](
        implicit
        rs: RSelector.Aux[S, H, V],
        ss: SubSchema.Aux[S, T, Out0]
    ) = inhabit_Type[S, H::T, Field[H, V]::Out0](
        (s: S) => field[H](rs(s)) :: ss(s)
    )
}
trait LowPrioritySubSchema extends LowerPrioritySubSchema {
    implicit def subschema_head_is_field[S <: HList, K, V, T <: HList, Out0 <: HList](
        implicit
        rs: RSelector.Aux[S, K, V],
        ss: SubSchema.Aux[S, T, Out0]
    ) = inhabit_Type[S, Field[K,V]::T, Field[K,V]::Out0](
        (s: S) => field[K](rs(s)) :: ss(s)
    )
}
object SubSchema extends LowPrioritySubSchema {
    def apply[S <: HList, SS <: HList](implicit ok: SubSchema[S, SS]): Aux[S, SS, ok.Out] = ok

    implicit def subschema_head_is_nested_field[S <: HList, K, V <: HList, HH, HT <: HList, T <: HList, Out0 <: HList, Out1 <: HList](
        implicit
        rs: RSelector.Aux[S, K, V],
        ss1: SubSchema.Aux[V, HH::HT, Out0],
        ss2: SubSchema.Aux[S, T, Out1]
    ) = inhabit_Type[S, Field[K, HH::HT]::T, Field[K, Out0]::Out1](
        (s: S) => field[K](ss1(rs(s))) :: ss2(s)
    )

    implicit def subschema_is_hnil[S <: HList] = inhabit_Type[S, HNil, HNil]((s: S) => HNil)
}