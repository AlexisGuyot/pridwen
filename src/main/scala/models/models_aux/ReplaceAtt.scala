package pridwen.models.aux

import shapeless.{HList, HNil, ::}
import shapeless.ops.hlist.{Replacer}
import shapeless.labelled.{FieldType => Field, field}

import pridwen.support.{RSelector}

trait ReplaceAtt[S <: HList, P <: HList, NAN, NAT] { type Out <: HList ; def apply(s: S, na: NAT): Out }
object ReplaceAtt {
    def apply[S <: HList, P <: HList, NAN, NAT](implicit ok: ReplaceAtt[S, P, NAN, NAT]): Aux[S, P, NAN, NAT, ok.Out] = ok
    type Aux[S <: HList, P <: HList, NAN, NAT, Out0 <: HList] = ReplaceAtt[S, P, NAN, NAT] { type Out = Out0 }
    protected def inhabit_Type[S <: HList, P <: HList, NAN, NAT, Out0 <: HList](f: (S, NAT) => Out0): Aux[S, P, NAN, NAT, Out0] = new ReplaceAtt[S, P, NAN, NAT] { type Out = Out0 ; def apply(s: S, na: NAT) = f(s, na) }

    implicit def recurse_path[S <: HList, H, T <: HList, NAN, NAT, K, V <: HList, Out0 <: HList, Out1 <: HList](
        implicit
        rs: RSelector.Aux[S, H, K, V],
        sa: ReplaceAtt.Aux[V, T, NAN, NAT, Out0],
        r: Replacer.Aux[S, Field[K, V], Field[K, Out0], (Field[K, V], Out1)]
    ) = inhabit_Type[S, H::T, NAN, NAT, Out1](
        (s: S, na: NAT) => r(s, field[K](sa(rs(s), na)))._2
    )

    implicit def stop_recursion[S <: HList, H, K, V, NAN, NAT, Out0 <: HList](
        implicit
        rs: RSelector.Aux[S, H, K, V],
        r: Replacer.Aux[S, Field[K,V], Field[NAN, NAT], (Field[K,V], Out0)]
    ) = inhabit_Type[S, H::HNil, NAN, NAT, Out0](
        (s: S, na: NAT) => r(s, field[NAN](na))._2
    )
}