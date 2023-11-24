package pridwen.operators

import shapeless.{HList, HNil, Witness}

import pridwen.support.functions.{getFieldValue}
import pridwen.models._
import pridwen.models.aux.{JoinSchema, JoinMode, Index}

import scala.collection.mutable.{Map}

object join {
    def join [
        LeftModel <: Model, RightModel <: Model,
        Path_To_Left_Key, Path_To_Right_Key, KeyType,
        New_Schema <: HList, Out
    ](
        ldataset: LeftModel, rdataset: RightModel,
        lkey: Path_To_Left_Key, rkey: Path_To_Right_Key,
    )(
        implicit
        compute_join: JoinSchema.Aux[ldataset.Schema, rdataset.Schema, Path_To_Left_Key, Path_To_Right_Key, JoinMode.Default, New_Schema],
        lindex: Index.Aux[ldataset.Schema, Path_To_Left_Key, Map[KeyType, List[ldataset.Schema]]],
        rindex: Index.Aux[rdataset.Schema, Path_To_Right_Key, Map[KeyType, List[rdataset.Schema]]]
    ) = new {
        def apply(implicit same: LeftModel =:= RightModel, new_dataset: Model.As.Aux[New_Schema, LeftModel, Out]): Out
            = new_dataset(compute_join.with_index(lindex(ldataset.data), rindex(rdataset.data)))

        def to[ModelOut <: Model](implicit new_dataset: Model.As.Aux[New_Schema, ModelOut, Out]): Out
            = new_dataset(compute_join.with_index(lindex(ldataset.data), rindex(rdataset.data)))
    }


    def join_in_left [
        LeftModel <: Model, RightModel <: Model,
        Path_To_Left_Key, Path_To_Right_Key, KeyType,
        New_Schema <: HList, Out
    ](
        ldataset: LeftModel, rdataset: RightModel,
        lkey: Path_To_Left_Key, rkey: Path_To_Right_Key,
    )(
        implicit
        compute_join: JoinSchema.Aux[ldataset.Schema, rdataset.Schema, Path_To_Left_Key, Path_To_Right_Key, JoinMode.InLeft, New_Schema],
        lindex: Index.Aux[ldataset.Schema, Path_To_Left_Key, Map[KeyType, List[ldataset.Schema]]],
        rindex: Index.Aux[rdataset.Schema, Path_To_Right_Key, Map[KeyType, List[rdataset.Schema]]],
        new_dataset: Model.As.Aux[New_Schema, LeftModel, Out]
    ): Out = new_dataset(compute_join.with_index(lindex(ldataset.data), rindex(rdataset.data)))


    def join_in_right [
        LeftModel <: Model, RightModel <: Model,
        Path_To_Left_Key, Path_To_Right_Key, KeyType,
        New_Schema <: HList, Out
    ](
        ldataset: LeftModel, rdataset: RightModel,
        lkey: Path_To_Left_Key, rkey: Path_To_Right_Key,
    )(
        implicit
        compute_join: JoinSchema.Aux[ldataset.Schema, rdataset.Schema, Path_To_Left_Key, Path_To_Right_Key, JoinMode.InRight, New_Schema],
        lindex: Index.Aux[ldataset.Schema, Path_To_Left_Key, Map[KeyType, List[ldataset.Schema]]],
        rindex: Index.Aux[rdataset.Schema, Path_To_Right_Key, Map[KeyType, List[rdataset.Schema]]],
        new_dataset: Model.As.Aux[New_Schema, RightModel, Out]
    ): Out = new_dataset(compute_join.with_index(lindex(ldataset.data), rindex(rdataset.data)))
}