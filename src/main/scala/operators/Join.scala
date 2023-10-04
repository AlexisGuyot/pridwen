package pridwen.operators

import shapeless.{HList, HNil, Witness}

import pridwen.support.functions.{getFieldValue}
import pridwen.models._
import pridwen.models.aux.{IsValidSchema, Join, joinMode}

object join {
    // Idées d'améliorations : Support graphes pour la fonction join, Support inner/left/right/full join instances.

    def join [
        LeftModel <: Model[_], RightModel <: Model[_],
        Path_To_Left_Key <: HList, Path_To_Right_Key <: HList,
        New_Schema <: HList, ModelOut <: Model[_]
    ](
        ldataset: LeftModel,
        rdataset: RightModel,
        lkey: Path_To_Left_Key,
        rkey: Path_To_Right_Key,
        mout: ModelOut
    )(
        implicit
        compute_join: Join.Aux[ldataset.Repr, rdataset.Repr, Path_To_Left_Key, Path_To_Right_Key, joinMode.Default, New_Schema],
        res_model: IsValidSchema[New_Schema, ModelOut, HNil]
    ): res_model.Out = res_model(compute_join(ldataset.data, rdataset.data))



    def join_in_left [
        LeftModel <: Model[_], RightModel <: Model[_],
        Path_To_Left_Key <: HList, Path_To_Right_Key <: HList,
        New_Schema <: HList, New_LeftSchema <: HList
    ](
        ldataset: LeftModel,
        rdataset: RightModel,
        lkey: Path_To_Left_Key,
        rkey: Path_To_Right_Key
    )(
        implicit
        compute_join: Join.Aux[ldataset.Repr, rdataset.Repr, Path_To_Left_Key, Path_To_Right_Key, joinMode.InLeft, New_Schema],
        res_model: IsValidSchema[New_Schema, LeftModel, HNil]
    ): res_model.Out = res_model(compute_join(ldataset.data, rdataset.data))

    def join_in_left [
        Schema, SourceID, DestID, RightModel <: Model[_],
        Path_To_Left_Key <: HList, Path_To_Right_Key <: HList,
        New_Schema <: HList, New_LeftSchema <: HList
    ](
        ldataset: Graph[Schema, SourceID, DestID],
        rdataset: RightModel,
        lkey: Path_To_Left_Key,
        rkey: Path_To_Right_Key,
        newgraph_sourceID: Witness,
        newgraph_destID: Witness
    )(
        implicit
        compute_join: Join.Aux[ldataset.Repr, rdataset.Repr, Path_To_Left_Key, Path_To_Right_Key, joinMode.InLeft, New_Schema],
        res_model: IsValidGraph[New_Schema, newgraph_sourceID.T, newgraph_destID.T]
    ): Graph.Aux[New_Schema, newgraph_sourceID.T, newgraph_destID.T, res_model.Repr] 
        = res_model(compute_join(ldataset.data, rdataset.data))




    def join_in_right [
        LeftModel <: Model[_], RightModel <: Model[_],
        Path_To_Left_Key <: HList, Path_To_Right_Key <: HList, 
        New_Schema <: HList, New_RightSchema <: HList
    ](
        ldataset: LeftModel,
        rdataset: RightModel,
        lkey: Path_To_Left_Key,
        rkey: Path_To_Right_Key
    )(
        implicit
        compute_join: Join.Aux[ldataset.Repr, rdataset.Repr, Path_To_Left_Key, Path_To_Right_Key, joinMode.InRight, New_Schema],
        res_model: IsValidSchema[New_Schema, LeftModel, HNil]
    ): res_model.Out = res_model(compute_join(ldataset.data, rdataset.data))

    def join_in_right [
        LeftModel <: Model[_], S, SID, DID, RightModel <: Model[_],
        Path_To_Left_Key <: HList, Path_To_Right_Key <: HList, 
        New_Schema <: HList, New_RightSchema <: HList
    ](
        ldataset: LeftModel,
        rdataset: Graph[S, SID, DID],
        lkey: Path_To_Left_Key,
        rkey: Path_To_Right_Key,
        newgraph_sourceID: Witness,
        newgraph_destID: Witness
    )(
        implicit
        compute_join: Join.Aux[ldataset.Repr, rdataset.Repr, Path_To_Left_Key, Path_To_Right_Key, joinMode.InRight, New_Schema],
        res_model: IsValidGraph[New_Schema, newgraph_sourceID.T, newgraph_destID.T]
    ): Graph.Aux[New_Schema, newgraph_sourceID.T, newgraph_destID.T, res_model.Repr] 
        = res_model(compute_join(ldataset.data, rdataset.data))
}