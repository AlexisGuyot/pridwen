package pridwen.support

import shapeless.{HList, ::, Witness}
import shapeless.labelled.{FieldType => Field}
import shapeless.ops.record.Selector

trait RSelector[H <: HList, F] { type K ; type V ; def apply(hlist: H): V }
trait LowPriorityRSelector {
    type Aux[H <: HList, F, K0, V0] = RSelector[H, F] { type K = K0 ; type V = V0 }

    protected def inhabit_Type[H <: HList, F, K0, V0](
        f: H => V0
    ): Aux[H, F, K0, V0] = new RSelector[H, F] { 
        type K = K0 ; type V = V0 
        def apply(hlist: H) = f(hlist) 
    }

    implicit def f_is_a_field_name[H <: HList, FName, FType](
        implicit
        select_field: Selector.Aux[H, FName, FType]
    ) = inhabit_Type[H, FName, FName, FType](
        (hlist: H) => select_field(hlist)
    )
}
object RSelector extends LowPriorityRSelector {    
    implicit def f_is_a_field[H <: HList, FName, FType](
        implicit
        select_field: Selector.Aux[H, FName, FType]
    ) = inhabit_Type[H, Field[FName,FType], FName, FType](
        (hlist: H) => select_field(hlist)
    )

    implicit def f_is_a_singleton_containing_a_field_name[H <: HList, FName, FType](
        implicit
        select_field: Selector.Aux[H, FName, FType],
    ) = inhabit_Type[H, Witness.Aux[FName], FName, FType](
        (hlist: H) => select_field(hlist)
    )
}