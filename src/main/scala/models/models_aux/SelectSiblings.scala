package pridwen.models.aux

import shapeless.{HList, HNil, ::}
import shapeless.ops.hlist.{Selector => HSelector, Reverse, IsHCons}

trait SelectSiblings[S <: HList, P <: HList] { type Out <: HList ; def apply(s: S): Out }
object SelectSiblings {
    def apply[S <: HList, P <: HList](implicit ok: SelectSiblings[S, P]): Aux[S, P, ok.Out] = ok
    type Aux[S <: HList, P <: HList, Out0 <: HList] = SelectSiblings[S, P] { type Out = Out0 }
    protected def inhabit_Type[S <: HList, P <: HList, Out0 <: HList](f: S => Out0): Aux[S, P, Out0] = new SelectSiblings[S, P] { type Out = Out0 ; def apply(s: S) = f(s) }

    implicit def path_longer_than_1[S <: HList, P <: HList, A, RP <: HList, RPH, RPT <: HList, NP <: HList, N, NS <: HList]
    (
        implicit
        r1: Reverse.Aux[P, RP],
        i: IsHCons.Aux[RP, RPH, RPT],
        r2: Reverse.Aux[RPT, NP],
        sa: SelectAtt.Aux[S, NP, N, NS]
    ) = inhabit_Type[S, P, NS](
        (s: S) => sa(s)
    )

    implicit def path_of_1[S <: HList, A](
        implicit
        s: HSelector[S, A]
    ) = inhabit_Type[S, A::HNil, S](
        (s: S) => s
    )
}