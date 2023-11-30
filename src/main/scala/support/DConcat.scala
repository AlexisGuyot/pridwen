package pridwen.support

import shapeless.{HList, HNil, ::}
import shapeless.labelled.{FieldType => Field, field}
import shapeless.ops.hlist.{Modifier, Selector => HSelector, Prepend}
import shapeless.ops.record.{Selector => RSelector}

import pridwen.support.functions.getFieldValue

// Appends to H1 the elements contained in H2 they are not already in H1 (Distinct Concat). Handles nesting. Output type is the resulting HList.
trait DConcat[H1 <: HList, H2 <: HList] { type Out <: HList ; def apply(in1: H1, in2: H2): Out }
object DConcat {
    type Aux[H1 <: HList, H2 <: HList, Concat <: HList] = DConcat[H1, H2] { type Out = Concat }

    protected def inhabit_Type[H1 <: HList, H2 <: HList, Concat <: HList](
        f: (H1, H2) => Concat
    ): Aux[H1, H2, Concat] = new DConcat[H1, H2] { type Out = Concat ; def apply(in1: H1, in2: H2) = f(in1, in2) }

    implicit def launch_terminal_recursion [
        H1 <: HList, H2 <: HList, 
        Concat <: HList
    ](
        implicit
        concat: DConcat_TR.Aux[H1, H2, Concat]
    ) = inhabit_Type[H1, H2, Concat](
        (in1: H1, in2: H2) => concat(in1, in2)
    )

    // Private inner type allowing the terminal recursion in H1.
    private trait DConcat_TR[H1 <: HList, H2 <: HList] { type Out <: HList ; def apply(in1: H1, in2: H2): Out }
    private trait LowerPriorityDConcat_TR {
        type Aux[H1 <: HList, H2 <: HList, Concat <: HList] = DConcat_TR[H1, H2] { type Out = Concat }

        protected def inhabit_Type[H1 <: HList, H2 <: HList, Concat <: HList](
            f: (H1, H2) => Concat
        ): Aux[H1, H2, Concat] 
            = new DConcat_TR[H1, H2] { 
                type Out = Concat 
                def apply(in1: H1, in2: H2) = f(in1, in2) 
        }

        implicit def in2_head_is_not_in_in1 [
            H1 <: HList, Head2, Tail2 <: HList, 
            NewH1 <: HList, Concat <: HList
        ](
            implicit
            append_head2: Prepend.Aux[H1, Head2::HNil, NewH1],
            concat_tail2: DConcat_TR.Aux[NewH1, Tail2, Concat]
        ) = inhabit_Type[H1, Head2::Tail2, Concat](
            (in1: H1, in2: (Head2::Tail2)) => concat_tail2(append_head2(in1, in2.head :: HNil), in2.tail)
        )
    }
    private trait LowPriorityDConcat_TR extends LowerPriorityDConcat_TR {

        // More priority than in2_head_is_not_in_in1
        implicit def in2_head_is_already_in_in1 [
            H1 <: HList, Head2, Tail2 <: HList, 
            Concat <: HList
        ](
            implicit
            select_field: HSelector[H1, Head2],
            concat_tail2: DConcat_TR.Aux[H1, Tail2, Concat]
        ) = inhabit_Type[H1, Head2::Tail2, Concat](
            (in1: H1, in2: (Head2::Tail2)) => concat_tail2(in1, in2.tail)
        )
    }
    private object DConcat_TR extends LowPriorityDConcat_TR {
        def apply[H1 <: HList, H2 <: HList](implicit ok: DConcat_TR[H1, H2]): Aux[H1, H2, ok.Out] = ok

        // More priority than element_is_already_in_in1
        implicit def in2_head_is_a_nested_field [
            H1 <: HList, FName, FTypeIn2 <: HList, Tail2 <: HList, 
            FTypeIn1 <: HList, ConcatNested <: HList, NewH1 <: HList, Concat <: HList
        ](
            implicit
            select_field: RSelector.Aux[H1, FName, FTypeIn1],
            concat_nested: DConcat.Aux[FTypeIn1, FTypeIn2, ConcatNested],
            update_in1: Modifier.Aux[H1, Field[FName, FTypeIn1], Field[FName, ConcatNested], (Field[FName, FTypeIn1], NewH1)],
            concat_tail2: DConcat_TR.Aux[NewH1, Tail2, Concat]
        ) = inhabit_Type[H1, Field[FName,FTypeIn2]::Tail2, Concat](
            (in1: H1, in2: (Field[FName,FTypeIn2]::Tail2)) => concat_tail2(
                update_in1(in1, (f: Field[FName, FTypeIn1]) => field[FName](concat_nested(getFieldValue(f), getFieldValue(in2.head))))._2, 
                in2.tail
            )
        )

        implicit def in2_is_an_empty_hlist[H1 <: HList] = inhabit_Type[H1, HNil, H1](
            (in1: H1, in2: HNil) => in1
        )
    }
}