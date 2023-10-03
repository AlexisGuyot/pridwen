package pridwen.operators.predefined

import collection.mutable.Map

import shapeless.{HList, ::, HNil, Witness => W}
import shapeless.labelled.{FieldType => Field, field}
import shapeless.ops.hlist.{IsHCons}

import pridwen.models._
import pridwen.models.aux.{SelectField, SelectManyFields, SelectSiblings}
import pridwen.support.functions.{getFieldValue}

object construct {
    def constructGraph [
        Schema, ModelIn[_] <: Model[_], 
        Path_To_Source_ID <: HList, SourceID_Name, NodeID_Type,
        Path_To_Dest_ID <: HList, DestID_Name, 
        Res_Schema <: HList
    ](
        dataset: ModelIn[Schema],
        source: Path_To_Source_ID,
        dest: Path_To_Dest_ID
    )(
        implicit
        get_source_id: SelectField.Aux[dataset.Repr, Path_To_Source_ID, SourceID_Name, NodeID_Type],
        get_dest_id: SelectField.Aux[dataset.Repr, Path_To_Dest_ID, DestID_Name, NodeID_Type],
        res_schema: Res_Schema =:= ((Field[SourceID_Name, NodeID_Type] :: HNil) :: (Field[DestID_Name, NodeID_Type] :: HNil) :: (Field[W.`'weight`.T, Int] :: HNil) :: HNil),
        res_model: ValidGraph[Res_Schema, SourceID_Name, DestID_Name]
    ): Graph.Aux[Res_Schema, SourceID_Name, DestID_Name, res_model.E, res_model.V] = {
        val d = dataset.data
                .groupBy(schema => (get_source_id(schema) :: HNil, get_dest_id(schema) :: HNil))
                .mapValues(_.size)
                .map { case (key, value) => key._1 :: key._2 :: (field[W.`'weight`.T](value)::HNil) :: HNil }
                .toList
        res_model(d.asInstanceOf[List[Res_Schema]])
    }
    
    def constructGraph [
        Schema, ModelIn[_] <: Model[_], 
        Path_To_Source_ID <: HList, SourceID_Name, NodeID_Type,
        Path_To_Dest_ID <: HList, DestID_Name, 
        Path_To_Source_Att <: HList, Source_Att <: HList, 
        Path_To_Dest_Att <: HList, Dest_Att <: HList, 
        Path_To_Edge_Att <: HList, Edge_Att <: HList,
        Res_Schema <: HList
    ](
        dataset: ModelIn[Schema],
        source: Path_To_Source_ID,
        dest: Path_To_Dest_ID,
        source_att: Path_To_Source_Att,
        dest_att: Path_To_Dest_Att,
        edge_att: Path_To_Edge_Att
    )(
        implicit
        get_source_id: SelectField.Aux[dataset.Repr, Path_To_Source_ID, SourceID_Name, NodeID_Type],
        get_dest_id: SelectField.Aux[dataset.Repr, Path_To_Dest_ID, DestID_Name, NodeID_Type],
        get_source_att: SelectManyFields.Aux[dataset.Repr, Path_To_Source_Att, Source_Att],
        get_dest_att: SelectManyFields.Aux[dataset.Repr, Path_To_Dest_Att, Dest_Att],
        get_edge_att: SelectManyFields.Aux[dataset.Repr, Path_To_Edge_Att, Edge_Att],
        res_schema: Res_Schema =:= ((Field[SourceID_Name, NodeID_Type] :: Source_Att) :: (Field[DestID_Name, NodeID_Type] :: Dest_Att) :: (Field[W.`'weight`.T, Int] :: Edge_Att) :: HNil),
        res_model: ValidGraph[Res_Schema, SourceID_Name, DestID_Name]
    ): Graph.Aux[Res_Schema, SourceID_Name, DestID_Name, res_model.E, res_model.V] = {
        val d = dataset.data
                .groupBy(schema => (get_source_id(schema) :: get_source_att(schema), get_dest_id(schema) :: get_dest_att(schema), get_edge_att(schema)))
                .mapValues(_.size)
                .map { case (key, value) => key._1 :: key._2 :: (field[W.`'weight`.T](value)::key._3) :: HNil }
                .toList
        res_model(d.asInstanceOf[List[Res_Schema]])
    }


    
    def constructRelation[Schema, ModelIn[_] <: Model[_]](
        dataset: ModelIn[Schema]
    )(
        implicit
        res_model: ValidRelation[dataset.Repr]
    ): Relation.Aux[dataset.Repr, res_model.Out] = res_model(dataset.data)

    def constructRelation [
        Schema, ModelIn[_] <: Model[_],
        Path_To_Relation_Att <: HList, Relation_Att <: HList
    ](
        dataset: ModelIn[Schema], 
        relation_att: Path_To_Relation_Att
    )(
        implicit
        get_relation_att: SelectManyFields.Aux[dataset.Repr, Path_To_Relation_Att, Relation_Att],
        res_model: ValidRelation[Relation_Att]
    ): Relation.Aux[Relation_Att, res_model.Out] = {
        val d = dataset.data.map(schema => get_relation_att(schema))
        res_model(d)
    }
    



    def constructJSON[Schema, ModelIn[_] <: Model[_]](
        dataset: ModelIn[Schema]
    )(
        implicit
        res_model: ValidJSON[dataset.Repr]
    ): JSON.Aux[dataset.Repr, res_model.Out] 
        = res_model(dataset.data)

    def constructJSON [
        Schema, ModelIn[_] <: Model[_], 
        Path_To_JSON_Att <: HList, JSON_Att <: HList
    ](
        dataset: ModelIn[Schema], 
        json_att: Path_To_JSON_Att
    )(
        implicit
        get_json_att: SelectManyFields.Aux[dataset.Repr, Path_To_JSON_Att, JSON_Att],
        res_model: ValidJSON[JSON_Att]
    ): JSON.Aux[JSON_Att, res_model.Out] = {
        val d = dataset.data.map(schema => get_json_att(schema))
        res_model(d)
    }
}