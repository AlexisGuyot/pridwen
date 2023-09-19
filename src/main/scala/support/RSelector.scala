package pridwen.support

import shapeless.{HList, HNil, ::, Witness}
import shapeless.labelled.{FieldType => Field}
import shapeless.ops.record.{Selector}

trait RSelector[L <: HList, P] { type K ; type V ; def apply(l: L): V }
trait LowPriorityRSelector {
    type Aux[L <: HList, P, K0, V0] = RSelector[L, P] { type K = K0 ; type V = V0 }
    protected def inhabit_Type[L <: HList, P, K0, V0](f: L => V0): Aux[L, P, K0, V0] = new RSelector[L, P] { type K = K0 ; type V = V0 ; def apply(l: L) = f(l) }

    implicit def p_is_field_name[L <: HList, K, V](
        implicit
        s: Selector.Aux[L, K, V]
    ) = inhabit_Type[L, K, K, V](
        (l: L) => s(l)
    )
}
object RSelector extends LowPriorityRSelector {
    def apply[L <: HList, P](implicit ok: RSelector[L, P]): Aux[L, P, ok.K, ok.V] = ok
    
    implicit def p_is_field[L <: HList, K, V](
        implicit
        s: Selector.Aux[L, K, V]
    ) = inhabit_Type[L, Field[K,V], K, V](
        (l: L) => s(l)
    )

    implicit def p_is_witness[L <: HList, W <: Witness, V](
        implicit
        s: Selector.Aux[L, W#T, V]
    ) = inhabit_Type[L, W, W#T, V](
        (l: L) => s(l)
    )
}