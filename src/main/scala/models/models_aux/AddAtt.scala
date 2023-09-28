package pridwen.models.aux

import shapeless.{HList, ::, HNil}
import shapeless.labelled.{FieldType => Field, field}
import shapeless.ops.hlist.{Replacer, Prepend}

import pridwen.support.{RSelector}

trait AddAtt[S <: HList, P <: HList, AN, AT] { type Out <: HList ; def apply(s: S, a: AT): Out }
object AddAtt {
    type Aux[S <: HList, P <: HList, AN, AT, Out0 <: HList] = AddAtt[S, P, AN, AT] { type Out = Out0 }
    protected def inhabit_Type[S <: HList, P <: HList, AN, AT, Out0 <: HList](f: (S, AT) => Out0): Aux[S, P, AN, AT, Out0] = new AddAtt[S, P, AN, AT] { type Out = Out0 ; def apply(s: S, a: AT) = f(s, a) }
    def apply[S <: HList, P <: HList, AN, AT](implicit ok: AddAtt[S, P, AN, AT]): Aux[S, P, AN, AT, ok.Out] = ok

    implicit def recurse_path[S <: HList, H, T <: HList, AN, AT, K, V <: HList, Out0 <: HList, Out1 <: HList](
        implicit
        rs: RSelector.Aux[S, H, K, V],
        ad: AddAtt.Aux[V, T, AN, AT, Out0],
        r: Replacer.Aux[S, Field[K, V], Field[K, Out0], (Field[K, V], Out1)]
    ) = inhabit_Type[S, H::T, AN, AT, Out1](
        (s: S, a: AT) => r(s, field[K](ad(rs(s), a)))._2
    )

    implicit def add_field[S <: HList, Out0 <: HList, AN, AT](
        implicit
        p: Prepend.Aux[S, Field[AN, AT]::HNil, Out0]
    ) = inhabit_Type[S, HNil, AN, AT, Out0](
        (s: S, a: AT) => p(s, field[AN](a)::HNil)
    )
}