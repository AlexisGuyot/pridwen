package pridwen.models.aux

import shapeless.{HList, HNil, ::, Witness, Lazy}
import shapeless.labelled.{FieldType => Field, field}
import shapeless.ops.hlist.{Selector => HSelector}
//import shapeless.ops.record.{Selector => RSelector}

import pridwen.models.{Model}
import pridwen.support.{RSelector}
import pridwen.support.functions.{getFieldValue}

trait As[K, P <: HList]
object As {
    def apply[P <: HList](k: Witness, p: P) = new As[k.T, P] {}
}

trait SelectAtt[M, P] { type KOut ; type VOut ; def apply(in: M): Field[KOut, VOut] }
object SelectAtt {
    def apply[M, P](implicit ok: SelectAtt[M, P]): Aux[M, P, ok.KOut, ok.VOut] = ok
    type Aux[M, P, KOut0, VOut0] = SelectAtt[M, P] { type KOut = KOut0 ; type VOut = VOut0 }
    protected def inhabit_Type[M, P, KOut0, VOut0](f: M => Field[KOut0, VOut0]): Aux[M, P, KOut0, VOut0] = new SelectAtt[M, P] { type KOut = KOut0 ; type VOut = VOut0 ; def apply(in: M) = f(in) }


    implicit def recurse_path[S <: HList, H, T <: HList, K, V, KOut0, VOut0](
        implicit
        rs: RSelector.Aux[S, H, K, V],
        sa: SelectAtt.Aux[V, T, KOut0, VOut0]
    ) = inhabit_Type[S, H::T, KOut0, VOut0](
        (s: S) => sa(rs(s))
    )

    implicit def stop_recursion[S <: HList, H, K, V](
        implicit
        rs: RSelector.Aux[S, H, K, V]
    ) = inhabit_Type[S, H::HNil, K, V](
        (s: S) => field[K](rs(s))
    )

    implicit def with_alias[S <: HList, A <: Symbol, P <: HList, KOut0, VOut0](
        implicit
        sa: Lazy[SelectAtt.Aux[S, P, KOut0, VOut0]]
    ) = inhabit_Type[S, As[A, P], A, VOut0](
        (s: S) => field[A](getFieldValue(sa.value(s)))
    )

    implicit def field_name[S <: HList, A <: Symbol, K, V](
        implicit
        rs: RSelector.Aux[S, A, K, V]
    ) = inhabit_Type[S, A, K, V](
        (s: S) => field[K](rs(s))
    )
}