package pridwen.support

import shapeless.{HList, HNil, ::}
import shapeless.ops.hlist.{Reverse, IsHCons}

trait ReducePath[P <: HList] { type Out <: HList }
object ReducePath {
    def apply[P <: HList](implicit ok: ReducePath[P]): Aux[P, ok.Out] = ok
    type Aux[P <: HList, Out0 <: HList] = ReducePath[P] { type Out = Out0 }
    protected def inhabit_Type[P <: HList, Out0 <: HList]: Aux[P, Out0] = new ReducePath[P] { type Out = Out0 } 

    implicit def reduce_hlist[H, T <: HList, RP <: HList, RPH, RPT <: HList, NP <: HList](
        implicit
        r1: Reverse.Aux[H::T, RP],
        i: IsHCons.Aux[RP, RPH, RPT],
        r2: Reverse.Aux[RPT, NP]
    ) = inhabit_Type[H::T, NP]

    implicit def reduce_hnil = inhabit_Type[HNil, HNil]
}