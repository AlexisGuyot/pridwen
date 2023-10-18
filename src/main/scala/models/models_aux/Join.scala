package pridwen.models.aux

import shapeless.{HList, ::, HNil}

import pridwen.support.{ReducePath, DConcat}
import pridwen.support.functions.{getFieldValue}

import scala.collection.mutable.HashMap

trait And[Paths <: HList]
object And {
    def apply[Paths <: HList](p: Paths): And[Paths] = new And[Paths] {}
}
trait Or[Paths <: HList]
object Or {
    def apply[Paths <: HList](p: Paths): Or[Paths] = new Or[Paths] {}
}

object joinMode {
    trait JoinMode
    trait Default extends JoinMode
    trait InLeft extends JoinMode
    trait InRight extends JoinMode
}
import joinMode._

trait Join[LS <: HList, RS <: HList, LP, RP, M <: JoinMode] { type Out <: HList ; def apply(ldataset: List[LS], rdataset: List[RS]): List[Out] ; def apply2[K](ldataset: HashMap[K, List[LS]], rdataset: HashMap[K, List[RS]]): List[Out] }
object Join {
    def apply[LS <: HList, RS <: HList, LP, RP, M <: JoinMode](implicit ok: Join[LS, RS, LP, RP, M]): Aux[LS, RS, LP, RP, M, ok.Out] = ok
    type Aux[LS <: HList, RS <: HList, LP, RP, M <: JoinMode, Out0 <: HList] = Join[LS, RS, LP, RP, M] { type Out = Out0 }

    private def inhabit_Type[LS <: HList, RS <: HList, LP, RP, M <: JoinMode, Out0 <: HList](
        condition: (LS, RS) => Boolean,
        do_join: (LS, RS) => Out0
    ): Aux[LS, RS, LP, RP, M, Out0]
        = new Join[LS, RS, LP, RP, M] {
            type Out = Out0
            def apply(ldataset: List[LS], rdataset: List[RS]) = {
                val join_result = scala.collection.mutable.ListBuffer.empty[Out0]
                ldataset.foreach(lschema => rdataset.foreach(rschema => 
                    if(condition(lschema, rschema)) do_join(lschema, rschema) +=: join_result
                ))
                join_result.to(List)
            }

            def apply2[K](ldataset: HashMap[K, List[LS]], rdataset: HashMap[K, List[RS]]) = {
                val join_result = scala.collection.mutable.ListBuffer.empty[Out0]
                ldataset.keySet.intersect(rdataset.keySet).foreach(key => ldataset(key).foreach(lschema => rdataset(key).foreach(rschema =>
                    if(condition(lschema, rschema)) do_join(lschema, rschema) +=: join_result
                )))
                join_result.to(List)
            }
        }

    implicit def compute_join[LS <: HList, RS <: HList, LP, RP, M <: JoinMode, Out0 <: HList](
        implicit
        handle_andor: HandleAndOr.Aux[LS, RS, LP, RP, M, Out0]
    ) = inhabit_Type[LS, RS, LP, RP, M, Out0](handle_andor.condition, handle_andor.do_join)
}

trait HandleAndOr[LS <: HList, RS <: HList, LP, RP, M <: JoinMode] { type Out <: HList ; def condition(lschema: LS, rschema: RS): Boolean ; def do_join(lschema: LS, rschema: RS): Out }
trait LowPriorityHandleAndOr {
    type Aux[LS <: HList, RS <: HList, LP, RP, M <: JoinMode, Out0 <: HList] = HandleAndOr[LS, RS, LP, RP, M] { type Out = Out0 }

    protected def inhabit_Type[LS <: HList, RS <: HList, LP, RP, M <: JoinMode, Out0 <: HList](
        condition0: (LS, RS) => Boolean,
        do_join0: (LS, RS) => Out0
    ): Aux[LS, RS, LP, RP, M, Out0]
        = new HandleAndOr[LS, RS, LP, RP, M] {
            type Out = Out0
            def condition(lschema: LS, rschema: RS) = condition0(lschema, rschema)
            def do_join(lschema: LS, rschema: RS) = do_join0(lschema, rschema)
        }

    //================== Right

    /* implicit def right_path_is_and [
        LS <: HList, RS <: HList, LP, RP <: HList, RT <: HList, M <: JoinMode, Out0 <: HList, Out1 <: HList
    ](
        implicit
        check_other_side: HandleAndOr.Aux[LS, RS, LP, RP, M, Out0],
        recurse_rp: HandleAndOr.Aux[LS, RS, LP, And[RT], M, Out1]
    ) = inhabit_Type[LS, RS, LP, And[RP::RT], M, Out0](
        (lschema: LS, rschema: RS) => check_other_side.condition(lschema, rschema) && recurse_rp.condition(lschema, rschema),
        check_other_side.do_join
    )

    implicit def right_path_is_and_eor [
        LS <: HList, RS <: HList, LP, RP <: HList, M <: JoinMode, Out0 <: HList
    ](
        implicit
        check_other_side: HandleAndOr.Aux[LS, RS, LP, RP, M, Out0]
    ) = inhabit_Type[LS, RS, LP, And[RP::HNil], M, Out0](check_other_side.condition, check_other_side.do_join)

    implicit def right_path_is_or [
        LS <: HList, RS <: HList, LP, RP <: HList, RT <: HList, M <: JoinMode, Out0 <: HList, Out1 <: HList, Out2 <: HList
    ](
        implicit
        check_other_side: HandleAndOr.Aux[LS, RS, LP, RP, M, Out0],
        recurse_rp: HandleAndOr.Aux[LS, RS, LP, Or[RT], M, Out1],
        concat: DConcat.Aux[Out0, Out1, Out2]
    ) = inhabit_Type[LS, RS, LP, Or[RP::RT], M, Out2](
        (lschema: LS, rschema: RS) => check_other_side.condition(lschema, rschema) || recurse_rp.condition(lschema, rschema),
        (lschema: LS, rschema: RS) => concat(check_other_side.do_join(lschema, rschema), recurse_rp.do_join(lschema, rschema))
    )

    implicit def right_path_is_or_eor [
        LS <: HList, RS <: HList, LP, RP <: HList, M <: JoinMode, Out0 <: HList
    ](
        implicit
        check_other_side: HandleAndOr.Aux[LS, RS, LP, RP, M, Out0]
    ) = inhabit_Type[LS, RS, LP, Or[RP::HNil], M, Out0](check_other_side.condition, check_other_side.do_join) */
}
object HandleAndOr extends LowPriorityHandleAndOr {
    def apply[LS <: HList, RS <: HList, LP, RP, M <: JoinMode](implicit ok: HandleAndOr[LS, RS, LP, RP, M]): Aux[LS, RS, LP, RP, M, ok.Out] = ok

    //================== Default

    implicit def one_path_each_side [
        LS <: HList, RS <: HList, LP <: HList, RP <: HList, M <: JoinMode, Out0 <: HList
    ](
        implicit
        handle_mode: HandleJoinMode.Aux[LS, RS, LP, RP, M, Out0]
    ) = inhabit_Type[LS, RS, LP, RP, M, Out0](handle_mode.condition, handle_mode.do_join)

    //================== Left

    /* implicit def left_path_is_and [
        LS <: HList, RS <: HList, LP <: HList, LT <: HList, RP, M <: JoinMode, Out0 <: HList, Out1 <: HList
    ](
        implicit
        check_other_side: HandleAndOr.Aux[LS, RS, LP, RP, M, Out0],
        recurse_lp: HandleAndOr.Aux[LS, RS, And[LT], RP, M, Out1]
    ) = inhabit_Type[LS, RS, And[LP::LT], RP, M, Out0](
        (lschema: LS, rschema: RS) => check_other_side.condition(lschema, rschema) && recurse_lp.condition(lschema, rschema),
        check_other_side.do_join
    )

    implicit def left_path_is_and_eor [
        LS <: HList, RS <: HList, LP <: HList, RP, M <: JoinMode, Out0 <: HList
    ](
        implicit
        check_other_side: HandleAndOr.Aux[LS, RS, LP, RP, M, Out0]
    ) = inhabit_Type[LS, RS, And[LP::HNil], RP, M, Out0](check_other_side.condition, check_other_side.do_join)

    implicit def left_path_is_or [
        LS <: HList, RS <: HList, LP <: HList, LT <: HList, RP, M <: JoinMode, Out0 <: HList, Out1 <: HList
    ](
        implicit
        check_other_side: HandleAndOr.Aux[LS, RS, LP, RP, M, Out0],
        recurse_lp: HandleAndOr.Aux[LS, RS, Or[LT], RP, M, Out1]
    ) = inhabit_Type[LS, RS, Or[LP::LT], RP, M, Out0](
        (lschema: LS, rschema: RS) => check_other_side.condition(lschema, rschema) || recurse_lp.condition(lschema, rschema),
        check_other_side.do_join
    )

    implicit def left_path_is_or_eor [
        LS <: HList, RS <: HList, LP <: HList, RP, M <: JoinMode, Out0 <: HList
    ](
        implicit
        check_other_side: HandleAndOr.Aux[LS, RS, LP, RP, M, Out0]
    ) = inhabit_Type[LS, RS, Or[LP::HNil], RP, M, Out0](check_other_side.condition, check_other_side.do_join) */
}

trait HandleJoinMode[LS <: HList, RS <: HList, LP <: HList, RP <: HList, M <: JoinMode] { type Out <: HList ; def condition(lschema: LS, rschema: RS): Boolean ; def do_join(lschema: LS, rschema: RS): Out }
object HandleJoinMode {
    def apply[LS <: HList, RS <: HList, LP <: HList, RP <: HList, M <: JoinMode](implicit ok: HandleJoinMode[LS, RS, LP, RP, M]): Aux[LS, RS, LP, RP, M, ok.Out] = ok
    type Aux[LS <: HList, RS <: HList, LP <: HList, RP <: HList, M <: JoinMode, Out0 <: HList] = HandleJoinMode[LS, RS, LP, RP, M] { type Out = Out0 }

    private def inhabit_Type[LS <: HList, RS <: HList, LP <: HList, RP <: HList, M <: JoinMode, Out0 <: HList](
        condition0: (LS, RS) => Boolean,
        do_join0: (LS, RS) => Out0
    ): Aux[LS, RS, LP, RP, M, Out0]
        = new HandleJoinMode[LS, RS, LP, RP, M] {
            type Out = Out0
            def condition(lschema: LS, rschema: RS) = condition0(lschema, rschema)
            def do_join(lschema: LS, rschema: RS) = do_join0(lschema, rschema)
        }

    implicit def default_join [
        LS <: HList, RS <: HList, LP <: HList, RP <: HList, Out0 <: HList
    ](
        implicit
        join: ComputeAuxFunctions.Aux[LS, RS, LP, RP, Out0]
    ) = inhabit_Type[LS, RS, LP, RP, Default, Out0](join.condition, join.do_join)

    implicit def join_in_left [
        LS <: HList, RS <: HList, LP <: HList, RP <: HList, Out0 <: HList,
        RLP <: HList, Out1 <: HList
    ](
        implicit
        join: ComputeAuxFunctions.Aux[LS, RS, LP, RP, Out0],
        reduced_left_path: ReducePath.Aux[LP, RLP],
        update_lschema: ReplaceField.Aux[LS, RLP, HNil, Out0, Out1],
    ) = inhabit_Type[LS, RS, LP, RP, InLeft, Out1](
        join.condition,
        (lschema: LS, rschema: RS) => update_lschema(lschema, join.do_join(lschema, rschema))
    )

    implicit def join_in_right [
        LS <: HList, RS <: HList, LP <: HList, RP <: HList, Out0 <: HList,
        RRP <: HList, Out1 <: HList
    ](
        implicit
        join: ComputeAuxFunctions.Aux[LS, RS, LP, RP, Out0],
        reduced_right_path: ReducePath.Aux[RP, RRP],
        update_rschema: ReplaceField.Aux[RS, RRP, HNil, Out0, Out1],
    ) = inhabit_Type[LS, RS, LP, RP, InRight, Out1](
        join.condition,
        (lschema: LS, rschema: RS) => update_rschema(rschema, join.do_join(lschema, rschema))
    )
}

trait ComputeAuxFunctions[LS <: HList, RS <: HList, LP <: HList, RP <: HList] { type Out <: HList ; def condition(lschema: LS, rschema: RS): Boolean ; def do_join(lschema: LS, rschema: RS): Out }
object ComputeAuxFunctions {
    def apply[LS <: HList, RS <: HList, LP <: HList, RP <: HList](implicit ok: ComputeAuxFunctions[LS, RS, LP, RP]): Aux[LS, RS, LP, RP, ok.Out] = ok
    type Aux[LS <: HList, RS <: HList, LP <: HList, RP <: HList, Out0 <: HList] = ComputeAuxFunctions[LS, RS, LP, RP] { type Out = Out0 }

    private def inhabit_Type[LS <: HList, RS <: HList, LP <: HList, RP <: HList, Out0 <: HList](
        condition0: (LS, RS) => Boolean,
        do_join0: (LS, RS) => Out0
    ): Aux[LS, RS, LP, RP, Out0]
        = new ComputeAuxFunctions[LS, RS, LP, RP] {
            type Out = Out0
            def condition(lschema: LS, rschema: RS) = condition0(lschema, rschema)
            def do_join(lschema: LS, rschema: RS) = do_join0(lschema, rschema)
        }

    implicit def compute_aux_functions [
        LeftSchema <: HList, RightSchema <: HList, Path_To_Left_Key <: HList, Path_To_Right_Key <: HList, 
        LeftKey, RightKey, KeyType,
        Siblings_LeftKey <: HList, Siblings_RightKey <: HList, JoinedSchema <: HList
    ](
        implicit
        get_left_key: SelectField.Aux[LeftSchema, Path_To_Left_Key, LeftKey, KeyType],
        get_right_key: SelectField.Aux[RightSchema, Path_To_Right_Key, RightKey, KeyType],
        get_lk_siblings: SelectSiblings.Aux[LeftSchema, Path_To_Left_Key, Siblings_LeftKey],
        get_rk_siblings: SelectSiblings.Aux[RightSchema, Path_To_Right_Key, Siblings_RightKey],
        concat_siblings: DConcat.Aux[Siblings_LeftKey, Siblings_RightKey, JoinedSchema]
    ) = inhabit_Type[LeftSchema, RightSchema, Path_To_Left_Key, Path_To_Right_Key, JoinedSchema](
        (lschema: LeftSchema, rschema: RightSchema) => getFieldValue(get_left_key(lschema)) == getFieldValue(get_right_key(rschema)),
        (lschema: LeftSchema, rschema: RightSchema) => concat_siblings(get_lk_siblings(lschema), get_rk_siblings(rschema))
    )
}