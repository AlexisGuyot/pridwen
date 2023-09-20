package pridwen.models.aux

import shapeless.{HList, ::, HNil, Lazy, DepFn1}
import shapeless.labelled.{FieldType => Field, field}
import shapeless.ops.record.{Selector => RSelector}

trait SelectManyAtt[S, SS <: HList] { type Out <: HList ; def apply(s: S): Out }
object SelectManyAtt {
    type Aux[S, SS <: HList, Out0 <: HList] = SelectManyAtt[S, SS] { type Out = Out0 }
    protected def inhabit_Type[S, SS <: HList, Out0 <: HList](f: S => Out0): Aux[S, SS, Out0] = new SelectManyAtt[S, SS] { type Out = Out0 ; def apply(s: S) = f(s) }
    def apply[S, SS <: HList](implicit ok: SelectManyAtt[S, SS]): Aux[S, SS, ok.Out] = ok

    implicit def recurse_SelectManyAtt[S, P, T <: HList, KOut0, VOut0, Out0 <: HList](
        implicit
        sa: SelectAtt.Aux[S, P, KOut0, VOut0],
        ss: SelectManyAtt.Aux[S, T, Out0]
    ) = inhabit_Type[S, P::T, Field[KOut0, VOut0]::Out0](
        (s: S) => sa(s) :: ss(s)
    )

    implicit def end_recursion1[S] = inhabit_Type[S, HNil.type, HNil]((s: S) => HNil) 
    implicit def end_recursion2[S] = inhabit_Type[S, HNil, HNil]((s: S) => HNil) 
}