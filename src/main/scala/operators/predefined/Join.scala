package pridwen.operators.predefined

import shapeless.{HList, HNil, Witness}
import shapeless.labelled.{FieldType => Field}
import shapeless.ops.hlist.{Prepend}
import shapeless.ops.record.{Renamer}

import pridwen.support.functions.{getFieldValue}
import pridwen.support.{ReducePath, UPrepend}
import pridwen.models._
import pridwen.models.aux.{SelectField, SelectSiblings, IsValidSchema, ReplaceField}
import pridwen.operators.aux.{CheckFName}

object join {
    // Idées d'améliorations : Utiliser pridwen.models.aux.UpdateSchema pour simplifier les implicites, Décider de s'il faut garder CheckFName ou non, Support graphes pour la fonction join, Support inner/left/right/full join instances.

    private def do_join[LeftSchema <: HList, RightSchema <: HList, LeftKey, RightKey, LeftKey_Type, OutSchema <: HList](
        left: List[LeftSchema],
        right: List[RightSchema],
        left_key: LeftSchema => Field[LeftKey,LeftKey_Type],
        right_key: RightSchema => Field[RightKey,LeftKey_Type],
        join: (LeftSchema, RightSchema) => OutSchema
    ): List[OutSchema] = {
        val d = scala.collection.mutable.ListBuffer.empty[OutSchema]
        left.foreach(lschema => right.foreach(rschema => 
            if(getFieldValue(left_key(lschema)) == getFieldValue(right_key(rschema))) join(lschema, rschema) +=: d
        ))
        d.to(List)
    }



    def join [
        LeftModel <: Model[_], RightModel <: Model[_],
        Path_To_Left_Key <: HList, Path_To_Right_Key <: HList,
        LeftKey, RightKey, KeyType, LeftKey2, RightKey2,
        Siblings_LeftKey <: HList, Siblings_RightKey <: HList,
        LeftSchema <: HList, RightSchema <: HList, New_Schema <: HList,
        ModelOut <: Model[_]
    ](
        ldataset: LeftModel,
        rdataset: RightModel,
        lkey: Path_To_Left_Key,
        rkey: Path_To_Right_Key,
        mout: ModelOut
    )(
        implicit
        get_left_key: SelectField.Aux[ldataset.Repr, Path_To_Left_Key, LeftKey, KeyType],
        get_right_key: SelectField.Aux[rdataset.Repr, Path_To_Right_Key, RightKey, KeyType],
        check_key_names: CheckFName.Aux[LeftKey, RightKey, LeftKey2, RightKey2],
        get_lk_siblings: SelectSiblings.Aux[ldataset.Repr, Path_To_Left_Key, Siblings_LeftKey],
        get_rk_siblings: SelectSiblings.Aux[rdataset.Repr, Path_To_Right_Key, Siblings_RightKey],
        rename_lk: Renamer.Aux[Siblings_LeftKey, LeftKey, LeftKey2, LeftSchema],
        rename_rk: Renamer.Aux[Siblings_RightKey, RightKey, RightKey2, RightSchema],
        concat_siblings: Prepend.Aux[LeftSchema, RightSchema, New_Schema],
        res_model: IsValidSchema[New_Schema, ModelOut, HNil]
    ): res_model.Out = {
        val d = do_join(ldataset.data, rdataset.data, get_left_key.apply, get_right_key.apply, (lschema: ldataset.Repr, rschema: rdataset.Repr) =>
            concat_siblings(rename_lk(get_lk_siblings(lschema)), rename_rk(get_rk_siblings(rschema)))
        )
        res_model(d)
    }



    def join_in_left [
        LeftModel <: Model[_], RightModel <: Model[_],
        Path_To_Left_Key <: HList, Path_To_Right_Key <: HList, Reduced_LPath <: HList,
        LeftKey, RightKey, KeyType, LeftKey2, RightKey2,
        Siblings_LeftKey <: HList, Siblings_RightKey <: HList,
        LeftSchema <: HList, RightSchema <: HList,
        New_Schema <: HList, New_LeftSchema <: HList
    ](
        ldataset: LeftModel,
        rdataset: RightModel,
        lkey: Path_To_Left_Key,
        rkey: Path_To_Right_Key
    )(
        implicit
        get_left_key: SelectField.Aux[ldataset.Repr, Path_To_Left_Key, LeftKey, KeyType],
        get_right_key: SelectField.Aux[rdataset.Repr, Path_To_Right_Key, RightKey, KeyType],
        check_key_names: CheckFName.Aux[LeftKey, RightKey, LeftKey2, RightKey2],
        get_lk_siblings: SelectSiblings.Aux[ldataset.Repr, Path_To_Left_Key, Siblings_LeftKey],
        get_rk_siblings: SelectSiblings.Aux[rdataset.Repr, Path_To_Right_Key, Siblings_RightKey],
        rename_lk: Renamer.Aux[Siblings_LeftKey, LeftKey, LeftKey2, LeftSchema],
        rename_rk: Renamer.Aux[Siblings_RightKey, RightKey, RightKey2, RightSchema],
        concat_siblings: Prepend.Aux[LeftSchema, RightSchema, New_Schema],
        reduced_left_path: ReducePath.Aux[Path_To_Left_Key, Reduced_LPath],
        update_lschema: ReplaceField.Aux[ldataset.Repr, Reduced_LPath, HNil, New_Schema, New_LeftSchema],
        res_model: IsValidSchema[New_LeftSchema, LeftModel, HNil]
    ): res_model.Out = {
        val d = do_join(ldataset.data, rdataset.data, get_left_key.apply, get_right_key.apply, (lschema: ldataset.Repr, rschema: rdataset.Repr) =>
            update_lschema(lschema, concat_siblings(rename_lk(get_lk_siblings(lschema)), rename_rk(get_rk_siblings(rschema))))
        )
        res_model(d)
    }

    def join_in_left [
        Schema, SourceID, DestID, RightModel <: Model[_],
        Path_To_Left_Key <: HList, Reduced_LPath <: HList, Path_To_Right_Key <: HList,
        LeftKey, RightKey, KeyType, LeftKey2, RightKey2,
        Siblings_LeftKey <: HList, Siblings_RightKey <: HList,
        LeftSchema <: HList, RightSchema <: HList,
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
        get_left_key: SelectField.Aux[ldataset.Repr, Path_To_Left_Key, LeftKey, KeyType],
        get_right_key: SelectField.Aux[rdataset.Repr, Path_To_Right_Key, RightKey, KeyType],
        get_lk_siblings: SelectSiblings.Aux[ldataset.Repr, Path_To_Left_Key, Siblings_LeftKey],
        get_rk_siblings: SelectSiblings.Aux[rdataset.Repr, Path_To_Right_Key, Siblings_RightKey],
        concat_siblings: UPrepend.Aux[Siblings_LeftKey, Siblings_RightKey, New_Schema],
        reduced_left_path: ReducePath.Aux[Path_To_Left_Key, Reduced_LPath],
        update_lschema: ReplaceField.Aux[ldataset.Repr, Reduced_LPath, HNil, New_Schema, New_LeftSchema],
        res_model: ValidGraph[New_LeftSchema, newgraph_sourceID.T, newgraph_destID.T]
    ): Graph.Aux[New_LeftSchema, newgraph_sourceID.T, newgraph_destID.T, res_model.E, res_model.V] = {
        val d = do_join(ldataset.data, rdataset.data, get_left_key.apply, get_right_key.apply, (lschema: ldataset.Repr, rschema: rdataset.Repr) =>
            update_lschema(lschema, concat_siblings(get_lk_siblings(lschema), get_rk_siblings(rschema)))
        )
        res_model(d)
    }




    def join_in_right [
        LeftModel <: Model[_], RightModel <: Model[_],
        Path_To_Left_Key <: HList, Path_To_Right_Key <: HList, Reduced_RPath <: HList, 
        LeftKey, RightKey, KeyType, LeftKey2, RightKey2,
        Siblings_LeftKey <: HList, Siblings_RightKey <: HList,
        LeftSchema <: HList, RightSchema <: HList,
        New_Schema <: HList, New_RightSchema <: HList
    ](
        ldataset: LeftModel,
        rdataset: RightModel,
        lkey: Path_To_Left_Key,
        rkey: Path_To_Right_Key
    )(
        implicit
        get_left_key: SelectField.Aux[ldataset.Repr, Path_To_Left_Key, LeftKey, KeyType],
        get_right_key: SelectField.Aux[rdataset.Repr, Path_To_Right_Key, RightKey, KeyType],
        get_lk_siblings: SelectSiblings.Aux[ldataset.Repr, Path_To_Left_Key, Siblings_LeftKey],
        get_rk_siblings: SelectSiblings.Aux[rdataset.Repr, Path_To_Right_Key, Siblings_RightKey],
        check_key_names: CheckFName.Aux[LeftKey, RightKey, LeftKey2, RightKey2],
        rename_lk: Renamer.Aux[Siblings_LeftKey, LeftKey, LeftKey2, LeftSchema],
        rename_rk: Renamer.Aux[Siblings_RightKey, RightKey, RightKey2, RightSchema],
        concat_siblings: Prepend.Aux[LeftSchema, RightSchema, New_Schema],
        reduced_right_path: ReducePath.Aux[Path_To_Right_Key, Reduced_RPath],
        update_rschema: ReplaceField.Aux[rdataset.Repr, Reduced_RPath, HNil, New_Schema, New_RightSchema],
        res_model: IsValidSchema[New_RightSchema, RightModel, HNil]
    ): res_model.Out = {
        val d = do_join(ldataset.data, rdataset.data, get_left_key.apply, get_right_key.apply, (lschema: ldataset.Repr, rschema: rdataset.Repr) =>
            update_rschema(rschema, concat_siblings(rename_lk(get_lk_siblings(lschema)), rename_rk(get_rk_siblings(rschema))))
        )
        res_model(d)
    }

    def join_in_right [
        LeftModel <: Model[_], S, SID, DID, RightModel <: Model[_],
        Path_To_Left_Key <: HList, Path_To_Right_Key <: HList, Reduced_RPath <: HList,
        LeftKey, RightKey, KeyType, LeftKey2, RightKey2,
        Siblings_LeftKey <: HList, Siblings_RightKey <: HList,
        LeftSchema <: HList, RightSchema <: HList,
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
        get_left_key: SelectField.Aux[ldataset.Repr, Path_To_Left_Key, LeftKey, KeyType],
        get_right_key: SelectField.Aux[rdataset.Repr, Path_To_Right_Key, RightKey, KeyType],
        get_lk_siblings: SelectSiblings.Aux[ldataset.Repr, Path_To_Left_Key, Siblings_LeftKey],
        get_rk_siblings: SelectSiblings.Aux[rdataset.Repr, Path_To_Right_Key, Siblings_RightKey],
        //check_key_names: CheckFName.Aux[LeftKey, RightKey, LeftKey2, RightKey2],
        //rename_lk: Renamer.Aux[Siblings_LeftKey, LeftKey, LeftKey2, LeftSchema],
        //rename_rk: Renamer.Aux[Siblings_RightKey, RightKey, RightKey2, RightSchema],
        //concat_siblings: Prepend.Aux[LeftSchema, RightSchema, New_Schema],
        concat_siblings: UPrepend.Aux[Siblings_LeftKey, Siblings_RightKey, New_Schema],
        reduced_right_path: ReducePath.Aux[Path_To_Right_Key, Reduced_RPath],
        update_rschema: ReplaceField.Aux[rdataset.Repr, Reduced_RPath, HNil, New_Schema, New_RightSchema],
        res_model: ValidGraph[New_RightSchema, newgraph_sourceID.T, newgraph_destID.T]
    ): Graph.Aux[New_RightSchema, newgraph_sourceID.T, newgraph_destID.T, res_model.E, res_model.V] = {
        val d = do_join(ldataset.data, rdataset.data, get_left_key.apply, get_right_key.apply, (lschema: ldataset.Repr, rschema: rdataset.Repr) =>
            //update_rschema(rschema, concat_siblings(rename_lk(get_lk_siblings(lschema)), rename_rk(get_rk_siblings(rschema))))
            update_rschema(rschema, concat_siblings(get_lk_siblings(lschema), get_rk_siblings(rschema)))
        )
        res_model(d)
    }
}