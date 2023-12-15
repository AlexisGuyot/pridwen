package pridwen.operators

import pridwen.models._
import pridwen.schemaop.{SelectField, SelectManyFields}

import shapeless.{HList, HNil, ::, Witness => W}
import shapeless.labelled.{FieldType => Field, field}

import scala.collection.parallel.CollectionConverters._
import scala.reflect.ClassTag

object construct {
    def construct_from[ModelIn <: Model](from: ModelIn) = new {
        def a[ModelOut <: Model] = new {
            def construct[Out](implicit new_dataset: Model.As.Aux[from.Schema, ModelOut, Out]): Out = new_dataset(from.data)
            def withAttributes[Path_To_Att <: HList, Att <: HList, Out](
                attributes: Path_To_Att
            )(
                implicit
                get_att: SelectManyFields.Aux[from.Schema, Path_To_Att, Att],
                new_dataset: Model.As.Aux[Att, ModelOut, Out]
            ): Out = new_dataset(from.data.par.map(schema => get_att(schema)).toList)
        }

        def aGraph[Path_To_Source_ID <: HList, Path_To_Dest_ID <: HList, SourceID_Name, DestID_Name, NodeID_Type](
            source: Path_To_Source_ID, 
            dest: Path_To_Dest_ID
        )(
            implicit
            get_source_id: SelectField.Aux[from.Schema, Path_To_Source_ID, SourceID_Name, NodeID_Type],
            get_dest_id: SelectField.Aux[from.Schema, Path_To_Dest_ID, DestID_Name, NodeID_Type]
        ) = new {
            def common[S <: HList, SID, DID, Repr <: HList, SRepr <: HList, DRepr <: HList, Out, SourceAtt <: HList, DestAtt <: HList, EdgeAtt <: HList](
                    new_dataset: Graph.ValidSchema.Aux[S, SID, DID, Repr, SRepr, DRepr, Out], 
                    source_att: from.Schema => SourceAtt, 
                    dest_att: from.Schema => DestAtt, 
                    edge_att: from.Schema => EdgeAtt
                )(
                    implicit
                    eq: S =:= ((Field[SourceID_Name, NodeID_Type] :: SourceAtt) :: (Field[DestID_Name, NodeID_Type] :: DestAtt) :: (Field[W.`'weight`.T, Int] :: EdgeAtt) :: HNil)
                ): Out = {
                    println("--- Edge aggregation (substep 1/2)")
                    val d: List[S] = time {
                        from.data.par
                            .groupBy(schema => (get_source_id(schema) :: source_att(schema), get_dest_id(schema) :: dest_att(schema), edge_att(schema)))
                            .mapValues(_.size)
                            .map { case (key, value) => eq.flip(key._1 :: key._2 :: (field[W.`'weight`.T](value) :: key._3) :: HNil) }
                            .toList }
                    println("--- Modelling data in a graph (substep 1/2)")
                    time { new_dataset(d) }
                }
                
                def construct[Repr <: HList, SRepr <: HList, DRepr <: HList, Out](
                    implicit graph: Graph.ValidSchema.Aux[(Field[SourceID_Name, NodeID_Type] :: HNil) :: (Field[DestID_Name, NodeID_Type] :: HNil) :: (Field[W.`'weight`.T, Int] :: HNil) :: HNil, SourceID_Name, DestID_Name, Repr, SRepr, DRepr, Out]
                ): Out = common(
                        graph,
                        (s: from.Schema) => HNil : HNil,
                        (s: from.Schema) => HNil : HNil,
                        (s: from.Schema) => HNil : HNil
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
                    get_source_att: SelectManyFields.Aux[from.Schema, Path_To_Source_Att, Source_Att],
                    get_dest_att: SelectManyFields.Aux[from.Schema, Path_To_Dest_Att, Dest_Att],
                    get_edge_att: SelectManyFields.Aux[from.Schema, Path_To_Edge_Att, Edge_Att],
                    graph: Graph.ValidSchema.Aux[(Field[SourceID_Name, NodeID_Type] :: Source_Att) :: (Field[DestID_Name, NodeID_Type] :: Dest_Att) :: (Field[W.`'weight`.T, Int] :: Edge_Att) :: HNil, SourceID_Name, DestID_Name, Repr, SRepr, DRepr, Out]
                ) = new {
                    def construct: Out = common(
                        graph, 
                        (s: from.Schema) => get_source_att(s) : Source_Att,
                        (s: from.Schema) => get_dest_att(s) : Dest_Att,
                        (s: from.Schema) => get_edge_att(s) : Edge_Att
                    )
                }
        }
    }

    // Source : https://biercoff.com/easily-measuring-code-execution-time-in-scala/
    private def time[R](block: => R): R = {
        val t0 = System.nanoTime()
        val result = block    // call-by-name
        val t1 = System.nanoTime()
        println("Elapsed time: " + (t1 - t0) + " ns")
        result
    } 
}