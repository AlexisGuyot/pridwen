package pridwen.operators.predefined

import shapeless.{HList, HNil, ::, Witness => W}
import shapeless.labelled.{FieldType => Field, field}

import pridwen.models._
import pridwen.models.aux.{SelectAtt, SelectManyAtt}
import pridwen.support.functions.{getFieldValue, rename}

object ops {
    def constructGraph[
        Schema, M[_] <: Model[_], ISchema <: HList,
        Path_To_Source_ID <: HList, SID_Name, NodeID_Type,
        Path_To_Dest_ID <: HList, DID_Name, 
        Path_To_Source_Att <: HList, Source_Att <: HList, 
        Path_To_Dest_Att <: HList, Dest_Att <: HList, 
        Path_To_Edge_Att <: HList, Edge_Att <: HList
    ](
        in: M[Schema],
        source: Path_To_Source_ID,
        dest: Path_To_Dest_ID,
        source_att: Path_To_Source_Att,
        dest_att: Path_To_Dest_Att,
        edge_att: Path_To_Edge_Att
    )(
        implicit
        internal_repr: GetModelRepr.Aux[M[Schema], ISchema],
        get_source_id: SelectAtt.Aux[ISchema, Path_To_Source_ID, SID_Name, NodeID_Type],
        get_dest_id: SelectAtt.Aux[ISchema, Path_To_Dest_ID, DID_Name, NodeID_Type],
        get_source_att: SelectManyAtt.Aux[ISchema, Path_To_Source_Att, Source_Att],
        get_dest_att: SelectManyAtt.Aux[ISchema, Path_To_Dest_Att, Dest_Att],
        get_edge_att: SelectManyAtt.Aux[ISchema, Path_To_Edge_Att, Edge_Att],
        res_model: Graph[(Field[W.`'id`.T, NodeID_Type] :: Source_Att) :: (Field[W.`'id`.T, NodeID_Type] :: Dest_Att) :: (Field[W.`'weight`.T, Int] :: Edge_Att) :: HNil, W.`'id`.T]
    ) = (dataset: Model.Aux[M, Schema, ISchema]) => {
            val d = dataset.data
                    .groupBy(schema => (rename(get_source_id(schema), W('id)) :: get_source_att(schema), rename(get_dest_id(schema), W('id)) :: get_dest_att(schema), get_edge_att(schema)))
                    .mapValues(_.size)
                    .map { case (key, value) => key._1 :: key._2 :: (field[W.`'weight`.T](value)::key._3) :: HNil }
                    .toList
            Graph.load(res_model)(d) 
    }



    
    def constructRelation[
        Schema, M[_] <: Model[_], ISchema <: HList
    ](
        in: M[Schema]
    )(
        implicit
        internal_repr: GetModelRepr.Aux[M[Schema], ISchema],
        res_model: Relation.Aux[ISchema, ISchema]
    ) = (dataset: Model.Aux[M, Schema, ISchema]) => Relation.load(res_model)(dataset.data)

    def constructRelation[
        Schema, M[_] <: Model[_], ISchema <: HList, 
        Path_To_Relation_Att <: HList, Relation_Att <: HList
    ](
        in: M[Schema], 
        relation_att: Path_To_Relation_Att
    )(
        implicit
        internal_repr: GetModelRepr.Aux[M[Schema], ISchema],
        get_relation_att: SelectManyAtt.Aux[ISchema, Path_To_Relation_Att, Relation_Att],
        res_model: Relation.Aux[Relation_Att, Relation_Att]
    ) = (dataset: Model.Aux[M, Schema, ISchema]) => {
        val d = dataset.data.map(schema => get_relation_att(schema))
        Relation.load(res_model)(d)
    }
    



    def constructJSON[
        Schema, M[_] <: Model[_], ISchema <: HList
    ](
        in: M[Schema]
    )(
        implicit
        internal_repr: GetModelRepr.Aux[M[Schema], ISchema],
        res_model: JSON.Aux[ISchema, ISchema]
    ) = (dataset: Model.Aux[M, Schema, ISchema]) => JSON.load(res_model)(dataset.data)

    def constructJSON[
        Schema, M[_] <: Model[_], ISchema <: HList, 
        Path_To_JSON_Att <: HList, JSON_Att <: HList
    ](
        in: M[Schema], 
        json_att: Path_To_JSON_Att
    )(
        implicit
        internal_repr: GetModelRepr.Aux[M[Schema], ISchema],
        get_json_att: SelectManyAtt.Aux[ISchema, Path_To_JSON_Att, JSON_Att],
        res_model: JSON.Aux[JSON_Att, JSON_Att]
    ) = (dataset: Model.Aux[M, Schema, ISchema]) => {
        val d = dataset.data.map(schema => get_json_att(schema))
        JSON.load(res_model)(d)
    }




    /* TO DO:
    - Add field
    - Add fields
    - Project / SubSchema
    - Join
    */

    /* def addField[S, M[_] <: Model[_], ISchema <: HList, Path <: HList, FType, PName, PType](m: M[S], path: Path, fname: Witness, f: List[S] => FType)(
        implicit
        in: GetModelRepr.Aux[M[S], ISchema],
        sa: SelectAtt.Aux[ISchema, Path, PName, PType],
        out: M[]
    ) */
}