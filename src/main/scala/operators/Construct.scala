package pridwen.operators

import pridwen.models._
import pridwen.models.aux.{SelectField, SelectManyFields}

import shapeless.{HList, HNil, ::, Witness => W}
import shapeless.labelled.{FieldType => Field, field}

import scala.collection.parallel.CollectionConverters._

object construct {
    def constructGraph [
        ModelIn <: Model, Path_To_Source_ID <: HList, Path_To_Dest_ID <: HList, 
        SourceID_Name, DestID_Name, NodeID_Type,
        Res_Schema <: HList
    ](
        dataset: ModelIn,
        source: Path_To_Source_ID,
        dest: Path_To_Dest_ID
    )(
        implicit
        get_source_id: SelectField.Aux[dataset.Schema, Path_To_Source_ID, SourceID_Name, NodeID_Type],
        get_dest_id: SelectField.Aux[dataset.Schema, Path_To_Dest_ID, DestID_Name, NodeID_Type]
    ) = new {
        private def common[S <: HList, SID, DID, Repr <: HList, SRepr <: HList, DRepr <: HList, Out, SourceAtt <: HList, DestAtt <: HList, EdgeAtt <: HList](
            new_dataset: Graph.ValidSchema.Aux[S, SID, DID, Repr, SRepr, DRepr, Out], 
            source_att: dataset.Schema => SourceAtt, 
            dest_att: dataset.Schema => DestAtt, 
            edge_att: dataset.Schema => EdgeAtt
        )(
            implicit
            eq: S =:= ((Field[SourceID_Name, NodeID_Type] :: SourceAtt) :: (Field[DestID_Name, NodeID_Type] :: DestAtt) :: (Field[W.`'weight`.T, Int] :: EdgeAtt) :: HNil)
        ): Out = {
            println("Aggrégation arêtes")
            val d: List[S] = time {
                dataset.data.par
                    .groupBy(schema => (get_source_id(schema) :: source_att(schema), get_dest_id(schema) :: dest_att(schema), edge_att(schema)))
                    .mapValues(_.size)
                    .map { case (key, value) => eq.flip(key._1 :: key._2 :: (field[W.`'weight`.T](value) :: key._3) :: HNil) }
                    .toList }
            println("Chargement graphe")
            time { new_dataset(d) }
        }
        
        def apply[Repr <: HList, SRepr <: HList, DRepr <: HList, Out](
            implicit graph: Graph.ValidSchema.Aux[(Field[SourceID_Name, NodeID_Type] :: HNil) :: (Field[DestID_Name, NodeID_Type] :: HNil) :: (Field[W.`'weight`.T, Int] :: HNil) :: HNil, SourceID_Name, DestID_Name, Repr, SRepr, DRepr, Out]
        ): Out = common(
                graph,
                (s: dataset.Schema) => HNil : HNil,
                (s: dataset.Schema) => HNil : HNil,
                (s: dataset.Schema) => HNil : HNil
            )

        def withAttributes [
            Path_To_Source_Att <: HList, Path_To_Dest_Att <: HList, Path_To_Edge_Att <: HList, 
            Source_Att <: HList, Dest_Att <: HList, Edge_Att <: HList,
            Repr <: HList, SRepr <: HList, DRepr <: HList, Out
        ](
            source_att: Path_To_Source_Att,
            dest_att: Path_To_Dest_Att,
            edge_att: Path_To_Edge_Att
        )(
            implicit
            get_source_att: SelectManyFields.Aux[dataset.Schema, Path_To_Source_Att, Source_Att],
            get_dest_att: SelectManyFields.Aux[dataset.Schema, Path_To_Dest_Att, Dest_Att],
            get_edge_att: SelectManyFields.Aux[dataset.Schema, Path_To_Edge_Att, Edge_Att],
            graph: Graph.ValidSchema.Aux[(Field[SourceID_Name, NodeID_Type] :: Source_Att) :: (Field[DestID_Name, NodeID_Type] :: Dest_Att) :: (Field[W.`'weight`.T, Int] :: Edge_Att) :: HNil, SourceID_Name, DestID_Name, Repr, SRepr, DRepr, Out]
        ): Out = common(
            graph, 
            (s: dataset.Schema) => get_source_att(s) : Source_Att,
            (s: dataset.Schema) => get_dest_att(s) : Dest_Att,
            (s: dataset.Schema) => get_edge_att(s) : Edge_Att
        )
    }

    /* def constructGraph [
        ModelIn <: Model, Path_To_Source_ID <: HList, Path_To_Dest_ID <: HList, 
        SourceID_Name, DestID_Name, NodeID_Type,
        Res_Schema <: HList
    ](
        dataset: ModelIn,
        source: Path_To_Source_ID,
        dest: Path_To_Dest_ID
    )(
        implicit
        get_source_id: SelectField.Aux[dataset.Schema, Path_To_Source_ID, SourceID_Name, NodeID_Type],
        get_dest_id: SelectField.Aux[dataset.Schema, Path_To_Dest_ID, DestID_Name, NodeID_Type],
        res_schema: Res_Schema =:= ((Field[SourceID_Name, NodeID_Type] :: HNil) :: (Field[DestID_Name, NodeID_Type] :: HNil) :: (Field[W.`'weight`.T, Int] :: HNil) :: HNil),
        graph: Graph.ValidSchema[Res_Schema, SourceID_Name, DestID_Name]
    ): graph.T = {
        println("Aggrégation arêtes")
        val d: List[Res_Schema] = time { dataset.data.par
                .groupBy(schema => (get_source_id(schema) :: HNil, get_dest_id(schema) :: HNil))
                .mapValues(_.size)
                .map { case (key, value) => res_schema.flip(key._1 :: key._2 :: (field[W.`'weight`.T](value)::HNil) :: HNil) }
                .toList }
        println("Chargement graphe")
        time { graph(d) }
    }

    def constructGraph [
        ModelIn <: Model, Path_To_Source_ID <: HList, Path_To_Dest_ID <: HList, 
        SourceID_Name, DestID_Name, NodeID_Type,
        Path_To_Source_Att <: HList, Path_To_Dest_Att <: HList, Path_To_Edge_Att <: HList, 
        Source_Att <: HList, Dest_Att <: HList, Edge_Att <: HList,
        Res_Schema <: HList
    ](
        dataset: ModelIn,
        source: Path_To_Source_ID,
        dest: Path_To_Dest_ID,
        source_att: Path_To_Source_Att,
        dest_att: Path_To_Dest_Att,
        edge_att: Path_To_Edge_Att
    )(
        implicit
        get_source_id: SelectField.Aux[dataset.Schema, Path_To_Source_ID, SourceID_Name, NodeID_Type],
        get_dest_id: SelectField.Aux[dataset.Schema, Path_To_Dest_ID, DestID_Name, NodeID_Type],
        get_source_att: SelectManyFields.Aux[dataset.Schema, Path_To_Source_Att, Source_Att],
        get_dest_att: SelectManyFields.Aux[dataset.Schema, Path_To_Dest_Att, Dest_Att],
        get_edge_att: SelectManyFields.Aux[dataset.Schema, Path_To_Edge_Att, Edge_Att],
        res_schema: Res_Schema =:= ((Field[SourceID_Name, NodeID_Type] :: Source_Att) :: (Field[DestID_Name, NodeID_Type] :: Dest_Att) :: (Field[W.`'weight`.T, Int] :: Edge_Att) :: HNil),
        graph: Graph.ValidSchema[Res_Schema, SourceID_Name, DestID_Name]
    ): graph.T = {
        println("Aggrégation arêtes")
        val d: List[Res_Schema] = time { dataset.data.par
                .groupBy(schema => (get_source_id(schema) :: get_source_att(schema), get_dest_id(schema) :: get_dest_att(schema), get_edge_att(schema)))
                .mapValues(_.size)
                .map { case (key, value) => res_schema.flip(key._1 :: key._2 :: (field[W.`'weight`.T](value)::key._3) :: HNil) }
                .toList }
        println("Chargement graphe")
        time { graph(d) }
    } */


    def constructRelation[ModelIn <: Model](dataset: ModelIn)(
        implicit relation: Relation.ValidSchema[dataset.Schema]
    ): relation.T = relation(dataset.data)

    def constructRelation[ModelIn <: Model, Path_To_Att <: HList, Att <: HList](dataset: ModelIn, attributes: Path_To_Att)(
        implicit 
        get_att: SelectManyFields.Aux[dataset.Schema, Path_To_Att, Att],
        relation: Relation.ValidSchema[Att]
    ): relation.T = relation(dataset.data.par.map(schema => get_att(schema)).toList)
    


    def constructJSON[ModelIn <: Model](dataset: ModelIn)(
        implicit json: JSON.ValidSchema[dataset.Schema]
    ): json.T = json(dataset.data)

    def constructJSON[ModelIn <: Model, Path_To_Att <: HList, Att <: HList](dataset: ModelIn, attributes: Path_To_Att)(
        implicit 
        get_att: SelectManyFields.Aux[dataset.Schema, Path_To_Att, Att],
        json: JSON.ValidSchema[Att]
    ): json.T = json(dataset.data.par.map(schema => get_att(schema)).toList)


    // Source : https://biercoff.com/easily-measuring-code-execution-time-in-scala/
    private def time[R](block: => R): R = {
        val t0 = System.nanoTime()
        val result = block    // call-by-name
        val t1 = System.nanoTime()
        println("Elapsed time: " + (t1 - t0) + " ns")
        result
    } 
}