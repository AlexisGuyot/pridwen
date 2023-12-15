package pridwen.schemaop

import shapeless.{HList, ::, HNil}

import scala.collection.mutable.Map
import scala.collection.parallel.CollectionConverters._ 

import pridwen.support.{ReducePath, DConcat}
import pridwen.support.functions.getFieldValue               

object JoinMode { trait JM ; trait Default extends JM ; trait InLeft extends JM ; trait InRight extends JM }
import JoinMode._

trait JoinSchema[LS <: HList, RS <: HList, LP, RP, M <: JM] { type Out <: HList ; def apply(ldataset: List[LS], rdataset: List[RS]): List[Out] ; def with_index[K](ldataset: Map[K, List[LS]], rdataset: Map[K, List[RS]]): List[Out] }
object JoinSchema {
    type Aux[LS <: HList, RS <: HList, LP, RP, M <: JM, Out0 <: HList] = JoinSchema[LS, RS, LP, RP, M] { type Out = Out0 }

    private def inhabit_Type[LS <: HList, RS <: HList, LP, RP, M <: JM, Out0 <: HList](
        condition: (LS, RS) => Boolean,
        do_join: (LS, RS) => Out0
    ): Aux[LS, RS, LP, RP, M, Out0] = new JoinSchema[LS, RS, LP, RP, M] {
        type Out = Out0
        def apply(ldataset: List[LS], rdataset: List[RS]) = {
            val join_result = scala.collection.mutable.ListBuffer.empty[Out0]
            ldataset.foreach(lschema => rdataset.foreach(rschema => if(condition(lschema, rschema)) do_join(lschema, rschema) +=: join_result ))
            join_result.to(List)
        }

        def with_index[K](ldataset: Map[K, List[LS]], rdataset: Map[K, List[RS]]) =
            ldataset.keySet.intersect(rdataset.keySet).par.flatMap(key => ldataset(key).flatMap(lschema => rdataset(key).foldLeft(List[Out]()){(acc, rschema) => if(condition(lschema, rschema)) do_join(lschema, rschema) :: acc else acc})).toList
    }

    implicit def default_join [
        LS <: HList, RS <: HList, LP <: HList, RP <: HList, Out0 <: HList
    ](
        implicit
        join: ComputeJoin.Aux[LS, RS, LP, RP, Out0]
    ) = inhabit_Type[LS, RS, LP, RP, Default, Out0](join.condition, join.do_join)

    implicit def join_in_left [
        LS <: HList, RS <: HList, LP <: HList, RP <: HList, Out0 <: HList,
        RLP <: HList, Out1 <: HList
    ](
        implicit
        join: ComputeJoin.Aux[LS, RS, LP, RP, Out0],
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
        join: ComputeJoin.Aux[LS, RS, LP, RP, Out0],
        reduced_right_path: ReducePath.Aux[RP, RRP],
        update_rschema: ReplaceField.Aux[RS, RRP, HNil, Out0, Out1],
    ) = inhabit_Type[LS, RS, LP, RP, InRight, Out1](
        join.condition,
        (lschema: LS, rschema: RS) => update_rschema(rschema, join.do_join(lschema, rschema))
    )

    private trait ComputeJoin[LS <: HList, RS <: HList, LP <: HList, RP <: HList] { type Out <: HList ; val condition: (LS, RS) => Boolean ; val do_join: (LS, RS) => Out }
    private object ComputeJoin {
        type Aux[LS <: HList, RS <: HList, LP <: HList, RP <: HList, Out0 <: HList] = ComputeJoin[LS, RS, LP, RP] { type Out = Out0 }

        private def inhabit_Type[LS <: HList, RS <: HList, LP <: HList, RP <: HList, Out0 <: HList](
            condition0: (LS, RS) => Boolean,
            do_join0: (LS, RS) => Out0
        ): Aux[LS, RS, LP, RP, Out0]
            = new ComputeJoin[LS, RS, LP, RP] { type Out = Out0 ; val condition = condition0 ; val do_join = do_join0 }

        implicit def compute_aux_functions [
            LeftSchema <: HList, RightSchema <: HList, 
            Path_To_Left_Key <: HList, Path_To_Right_Key <: HList, LeftKey, RightKey, KeyType,
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
}