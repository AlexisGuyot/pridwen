package pridwen.support

import shapeless.{HList, HNil, ::}
import shapeless.labelled.{FieldType, field}
import shapeless.ops.hlist.{Modifier, Selector => HSelector, Prepend}
import shapeless.ops.record.{Selector => RSelector}

import pridwen.support.functions.{getFieldValue}


// Appends to L1 the elements contained in L2 they are not already in L1. Output type is the resulting HList.
trait UPrepend[L1, L2] { type Out <: HList ; def apply(l1: L1, l2: L2): Out }
object UPrepend {
    type Aux[L1, L2, Out0 <: HList] = UPrepend[L1, L2] { type Out = Out0 }
    protected def inhabit_Type[L1, L2, Out0 <: HList](f: (L1, L2) => Out0): Aux[L1, L2, Out0] = new UPrepend[L1, L2] { type Out = Out0 ; def apply(l1: L1, l2: L2) = f(l1, l2) }
    def apply[L1, L2](implicit ok: UPrepend[L1, L2]): Aux[L1, L2, ok.Out] = ok

    implicit def launch_terminal_recursion [
        L1, L2, 
        Out0 <: HList, Out1 <: HList, 
        Out2 <: HList
    ](
        implicit
        t1: ToHList.Aux[L1, Out0],
        t2: ToHList.Aux[L2, Out1],
        p: UPrepend_RT.Aux[Out0, Out1, Out2]
    ) = inhabit_Type[L1, L2, Out2](
        (l1: L1, l2: L2) => p(t1(l1), t2(l2))
    )

    // Private inner type allowing the terminal recursion.
    private trait UPrepend_RT[L1 <: HList, L2 <: HList] { type Out <: HList ; def apply(l1: L1, l2: L2): Out }
    private trait LowestPriorityUPrepend_RT {
        type Aux[L1 <: HList, L2 <: HList, Out0 <: HList] = UPrepend_RT[L1, L2] { type Out = Out0 }
        protected def inhabit_Type[L1 <: HList, L2 <: HList, Out0 <: HList](f: (L1, L2) => Out0): Aux[L1, L2, Out0] = new UPrepend_RT[L1, L2] { type Out = Out0 ; def apply(l1: L1, l2: L2) = f(l1, l2) }

        implicit def element_is_invalid_type_hlist [
            L1 <: HList, T2 <: HList, 
            Out0 <: HList
        ](
            implicit
            p: UPrepend_RT.Aux[L1, T2, Out0]
        ) = inhabit_Type[L1, HList::T2, Out0](
            (l1: L1, l2: (HList::T2)) => p(l1, l2.tail)
        )

        implicit def element_is_not_in_l1 [
            L1 <: HList, H2, T2 <: HList, 
            Out0 <: HList, 
            Out1 <: HList
        ](
            implicit
            p1: Prepend.Aux[L1, H2 :: HNil, Out0],
            p2: UPrepend_RT.Aux[Out0, T2, Out1]
        ) = inhabit_Type[L1, H2::T2, Out1](
            (l1: L1, l2: (H2::T2)) => p2(p1(l1, l2.head :: HNil), l2.tail)
        )
    }
    private trait LowerPriorityUPrepend_RT extends LowestPriorityUPrepend_RT {
        // More priority than element_is_invalid_type_hlist
        implicit def l1_contains_invalid_type_hlist [
            L1 <: HList, L2 <: HList, 
            Out0 <: HList
        ](
            implicit
            s: HSelector[L1, HList],
            p: UPrepend_RT.Aux[L2, L1, Out0]
        ) = inhabit_Type[L1, L2, Out0](
            (l1: L1, l2: L2) => p(l2, l1)
        )

        // More priority than element_is_not_in_l1
        implicit def element_is_already_in_l1 [
            L1 <: HList, H2, T2 <: HList, 
            Out0 <: HList
        ](
            implicit
            s: HSelector[L1, H2],
            p: UPrepend_RT.Aux[L1, T2, Out0]
        ) = inhabit_Type[L1, H2::T2, Out0](
            (l1: L1, l2: (H2::T2)) => p(l1, l2.tail)
        )
    }
    private trait LowPriorityUPrepend_RT extends LowerPriorityUPrepend_RT {
        // More priority than element_is_already_in_l1
        implicit def element_is_a_nested_field [
            L1 <: HList, K2, V2 <: HList, T2 <: HList, 
            V1 <: HList, 
            Out0 <: HList, 
            Out1 <: HList, 
            Out2 <: HList
        ](
            implicit
            s: RSelector.Aux[L1, K2, V1],
            p1: UPrepend.Aux[V1, V2, Out0],
            m: Modifier.Aux[L1, FieldType[K2, V1], FieldType[K2, Out0], (FieldType[K2, V1], Out1)],
            p2: UPrepend_RT.Aux[Out1, T2, Out2]
        ) = inhabit_Type[L1, FieldType[K2,V2]::T2, Out2](
            (l1: L1, l2: (FieldType[K2,V2]::T2)) => p2(m(l1, (x: FieldType[K2, V1]) => field[K2](p1(getFieldValue(x), getFieldValue(l2.head))))._2, l2.tail)
        )
    }
    private object UPrepend_RT extends LowPriorityUPrepend_RT {
        def apply[L1 <: HList, L2 <: HList](implicit ok: UPrepend_RT[L1, L2]): Aux[L1, L2, ok.Out] = ok

        // More priority than element_is_a_nested_field
        implicit def element_is_a_product [
            L1 <: HList, A2, B2, T2 <: HList, 
            A1, B1, 
            Out0 <: HList, Out1 <: HList, 
            Out2 <: HList, 
            Out3 <: HList
        ](
            implicit
            s: SubTypeSelector.Aux[L1, Product2[_,_], Product2[A1, B1]],
            p1: UPrepend.Aux[A1, A2, Out0],
            p2: UPrepend.Aux[B1, B2, Out1],
            m: Modifier.Aux[L1, Product2[A1, B1], Product2[Out0, Out1], (Product2[A1, B1], Out2)],
            p3: UPrepend.Aux[Out2, T2, Out3]
        ) = inhabit_Type[L1, Product2[A2, B2]::T2, Out3](
            (l1: L1, l2: Product2[A2, B2]::T2) => p3(m(l1, (x: Product2[A1, B1]) => (p1(x._1, l2.head._1), p2(x._2, l2.head._2)))._2, l2.tail)
        )

        implicit def l2_is_empty[L1 <: HList] = inhabit_Type[L1, HNil, L1](
            (l1: L1, l2: HNil) => l1
        )
    }
}