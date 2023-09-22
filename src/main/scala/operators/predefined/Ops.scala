package pridwen.operators.predefined

import shapeless.{HList, HNil, ::, Witness => W}
import shapeless.labelled.{FieldType => Field, field}

import pridwen.models.alt._
import pridwen.models.aux.{SelectAtt, SelectManyAtt}
import pridwen.support.functions.{getFieldValue, rename}

object ops {
    def constructGraph[
        Schema, M[_] <: Model[_], 
        Path_To_Source_ID <: HList, SID_Name, NodeID_Type,
        Path_To_Dest_ID <: HList, DID_Name, 
        Path_To_Source_Att <: HList, Source_Att <: HList, 
        Path_To_Dest_Att <: HList, Dest_Att <: HList, 
        Path_To_Edge_Att <: HList, Edge_Att <: HList,
    ](
        dataset: M[Schema],
        source: Path_To_Source_ID,
        dest: Path_To_Dest_ID,
        source_att: Path_To_Source_Att,
        dest_att: Path_To_Dest_Att,
        edge_att: Path_To_Edge_Att
    )(
        implicit
        get_source_id: SelectAtt.Aux[dataset.Repr, Path_To_Source_ID, SID_Name, NodeID_Type],
        get_dest_id: SelectAtt.Aux[dataset.Repr, Path_To_Dest_ID, DID_Name, NodeID_Type],
        get_source_att: SelectManyAtt.Aux[dataset.Repr, Path_To_Source_Att, Source_Att],
        get_dest_att: SelectManyAtt.Aux[dataset.Repr, Path_To_Dest_Att, Dest_Att],
        get_edge_att: SelectManyAtt.Aux[dataset.Repr, Path_To_Edge_Att, Edge_Att],
        res_model: ValidGraph[(Field[SID_Name, NodeID_Type] :: Source_Att) :: (Field[DID_Name, NodeID_Type] :: Dest_Att) :: (Field[W.`'weight`.T, Int] :: Edge_Att) :: HNil, SID_Name, DID_Name]
    ) = {
        val d = dataset.data
                .groupBy(schema => (get_source_id(schema) :: get_source_att(schema), get_dest_id(schema) :: get_dest_att(schema), get_edge_att(schema)))
                .mapValues(_.size)
                .map { case (key, value) => key._1 :: key._2 :: (field[W.`'weight`.T](value)::key._3) :: HNil }
                .toList
        res_model(d)
    }



    
    def constructRelation[
        Schema, M[_] <: Model[_], ISchema <: HList,
        Repr0 <: HList
    ](
        dataset: M[Schema]
    )(
        implicit
        res_model: ValidRelation.Aux[dataset.Repr, Relation.Aux[dataset.Repr, Repr0]]
    ): Relation.Aux[dataset.Repr, Repr0] = res_model(dataset.data)

    def constructRelation[
        Schema, M[_] <: Model[_], ISchema <: HList, 
        Path_To_Relation_Att <: HList, Relation_Att <: HList,
        Repr0 <: HList
    ](
        dataset: M[Schema], 
        relation_att: Path_To_Relation_Att
    )(
        implicit
        get_relation_att: SelectManyAtt.Aux[dataset.Repr, Path_To_Relation_Att, Relation_Att],
        res_model: ValidRelation.Aux[Relation_Att, Relation.Aux[dataset.Repr, Repr0]]
    ): Relation.Aux[dataset.Repr, Repr0] = {
        val d = dataset.data.map(schema => get_relation_att(schema))
        res_model(d)
    }
    



    def constructJSON[
        Schema, M[_] <: Model[_], ISchema <: HList,
        Repr0 <: HList
    ](
        dataset: M[Schema]
    )(
        implicit
        res_model: ValidJSON.Aux[dataset.Repr, JSON.Aux[dataset.Repr, Repr0]]
    ): JSON.Aux[dataset.Repr, Repr0] = res_model(dataset.data)

    def constructJSON[
        Schema, M[_] <: Model[_], ISchema <: HList, 
        Path_To_JSON_Att <: HList, JSON_Att <: HList,
        Repr0 <: HList
    ](
        dataset: M[Schema], 
        json_att: Path_To_JSON_Att
    )(
        implicit
        get_json_att: SelectManyAtt.Aux[dataset.Repr, Path_To_JSON_Att, JSON_Att],
        res_model: ValidJSON.Aux[JSON_Att, JSON.Aux[dataset.Repr, Repr0]]
    ): JSON.Aux[dataset.Repr, Repr0] = {
        val d = dataset.data.map(schema => get_json_att(schema))
        res_model(d)
    }

    def join[
        ML[_] <: Model[_], SchemaL, MR[_] <: Model[_], SchemaR,
        Path_To_Left_Key <: HList, Path_To_Right_Key <: HList,
        MOut <: Model[_]
    ](
        ldataset: ML[SchemaL],
        rdataset: MR[SchemaR],
        lkey: Path_To_Left_Key,
        rkey: Path_To_Right_Key,
        mout: MOut
    ) = ???
}