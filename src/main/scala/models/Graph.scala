package pridwen.models

import shapeless.{HList, HNil, ::, Witness}
import shapeless.labelled.{FieldType, field}
import shapeless.ops.hlist.{Prepend}

import pridwen.support.{DeepGeneric, RSelector}
import pridwen.support.functions.{getFieldValue, get}
import pridwen.models.aux.{IsValidSchema}



// ========================= Type definition

abstract class Graph[Schema, SourceID, DestID](dataset: List[Schema]) extends Model[Schema](dataset)
object Graph { 
    type Aux[Schema, SourceID, DestID, Repr0 <: HList] = Graph[Schema, SourceID, DestID] { type Repr = Repr0 }
    def apply[Schema, SourceID, DestID](dataset: List[Schema])(implicit ok: IsValidGraph[Schema, SourceID, DestID]) = ok(dataset)
}



// ========================= To verify that a schema conforms to the graph model

trait IsValidGraph[Schema, SourceID, DestID] { type Repr <: HList ; def apply(dataset: List[Schema]): Graph.Aux[Schema, SourceID, DestID, Repr] }
trait LowPriorityIsValidGraph {
    type Aux[Schema, SourceID, DestID, Repr0 <: HList] = IsValidGraph[Schema, SourceID, DestID] { type Repr = Repr0 }

    protected def inhabit_Type[Schema, SourceID, DestID, Repr0 <: HList](
        f: Schema => Repr0
    ): Aux[Schema, SourceID, DestID, Repr0] 
        = new IsValidGraph[Schema, SourceID, DestID] { 
            type Repr = Repr0 
            def apply(dataset: List[Schema]) = new Graph[Schema, SourceID, DestID](dataset) { type Repr = Repr0 ; def toRepr(schema: Schema) = f(schema) } 
    }

    implicit def edge_list_without_edge_attributes[SourceSchema <: HList, DestSchema <: HList, SourceID, DestID]
    (
        implicit
        //check_source_model: ValidRelation[SourceSchema],
        //check_dest_model: ValidRelation[DestSchema],
        get_sourceID: RSelector[SourceSchema, SourceID],
        get_destID: RSelector[DestSchema, DestID],
    ) = inhabit_Type [
        SourceSchema :: DestSchema :: HNil, SourceID, DestID,
        FieldType[Witness.`'source`.T, SourceSchema] :: FieldType[Witness.`'dest`.T, DestSchema] :: FieldType[Witness.`'edge`.T, HNil] :: HNil
    ](
        (schema: SourceSchema :: DestSchema :: HNil) => field[Witness.`'source`.T](schema.head) :: field[Witness.`'dest`.T](schema.tail.head) :: field[Witness.`'edge`.T](HNil) :: HNil
    )
}
object IsValidGraph extends LowPriorityIsValidGraph {
    def apply[Schema, SourceID, DestID](implicit ok: IsValidGraph[Schema, SourceID, DestID]): Aux[Schema, SourceID, DestID, ok.Repr] = ok

    implicit def schema_as_case_class [
        CSchema <: Product, SourceID, DestID, 
        HSchema <: HList, Repr0 <: HList
    ](
        implicit
        convert: DeepGeneric.Aux[CSchema, HSchema],
        graph: IsValidGraph.Aux[HSchema, SourceID, DestID, Repr0]
    ) = inhabit_Type[CSchema, SourceID, DestID, Repr0](
        (schema: CSchema) => graph(List(convert.to(schema))).data.head
    )

    // (HS::TS) = SourceSchema ; (HD::TD) = DestSchema ; (HE::TE) = EdgeSchema
    implicit def edge_list_with_edge_attributes[HS, TS <: HList, HD, TD <: HList, HE, TE <: HList, SourceID, DestID]
    (
        implicit
        //check_source_model: ValidRelation[SourceSchema],
        //check_dest_model: ValidRelation[DestSchema],
        //check_edge_model: ValidRelation[EdgeSchema],
        get_sourceID: RSelector[(HS::TS), SourceID],
        get_destID: RSelector[(HD::TD), DestID],
    ) = inhabit_Type[
        (HS::TS) :: (HD::TD) :: (HE::TE) :: HNil, SourceID, DestID,
        FieldType[Witness.`'source`.T, (HS::TS)] :: FieldType[Witness.`'dest`.T, (HD::TD)] :: FieldType[Witness.`'edge`.T, (HE::TE)] :: HNil
    ](
        (schema: (HS::TS) :: (HD::TD) :: (HE::TE) :: HNil) => field[Witness.`'source`.T](schema.head) :: field[Witness.`'dest`.T](schema.tail.head) :: field[Witness.`'edge`.T](schema.tail.tail.head) :: HNil
    )

    implicit def edge_list_with_nested_fields[
        SourceKey, DestKey, EdgeKey, 
        SourceSchema <: HList, DestSchema <: HList, EdgeSchema <: HList, 
        SourceID, DestID
    ](
        implicit
        //check_source_model: ValidRelation[SourceSchema],
        //check_dest_model: ValidRelation[DestSchema],
        //check_source_model: ValidRelation[EdgeSchema],
        get_sourceID: RSelector[SourceSchema, SourceID],
        get_destID: RSelector[DestSchema, DestID],
    ) = inhabit_Type[
        FieldType[SourceKey, SourceSchema] :: FieldType[DestKey, DestSchema] :: FieldType[EdgeKey, EdgeSchema] :: HNil, SourceID, DestID, 
        FieldType[Witness.`'source`.T, SourceSchema] :: FieldType[Witness.`'dest`.T, DestSchema] :: FieldType[Witness.`'edge`.T, EdgeSchema] :: HNil
    ](
        (s: FieldType[SourceKey, SourceSchema] :: FieldType[DestKey, DestSchema] :: FieldType[EdgeKey, EdgeSchema] :: HNil) => field[Witness.`'source`.T](getFieldValue(s.head)) :: field[Witness.`'dest`.T](getFieldValue(s.tail.head)) :: field[Witness.`'edge`.T](getFieldValue(s.tail.tail.head)) :: HNil
    )
}



// ========================= To infer the schema(s) of nodes from the edge list representation

trait GetNodes[Repr <: HList, ModelOut <: Model[_]] { type Out ; def apply(dataset: List[Repr]): Out }
trait LowPriorityGetNodes {
    type Aux[Repr <: HList, ModelOut <: Model[_], Nodes_Schema] = GetNodes[Repr, ModelOut] { type Out = Nodes_Schema }

    protected def inhabit_Type[Repr <: HList, ModelOut <: Model[_], Nodes_Schema](
        f: List[Repr] => Nodes_Schema
    ): Aux[Repr, ModelOut, Nodes_Schema] 
    = new GetNodes[Repr, ModelOut] { 
        type Out = Nodes_Schema 
        def apply(r: List[Repr]) = f(r) 
    }

    implicit def source_schema_and_dest_schema_are_different [
        SourceSchema <: HList, DestSchema <: HList, EdgeSchema <: HList, 
        ModelOut <: Model[_]
    ](
        implicit
        source_model: IsValidSchema[SourceSchema, ModelOut, HNil],
        dest_model: IsValidSchema[DestSchema, ModelOut, HNil]
    ) = inhabit_Type[
        FieldType[Witness.`'source`.T, SourceSchema] :: FieldType[Witness.`'dest`.T, DestSchema] :: FieldType[Witness.`'edge`.T, EdgeSchema] :: HNil, 
        ModelOut, 
        (source_model.Out, dest_model.Out)
    ](
        (dataset: List[FieldType[Witness.`'source`.T, SourceSchema] :: FieldType[Witness.`'dest`.T, DestSchema] :: FieldType[Witness.`'edge`.T, EdgeSchema] :: HNil]) => {
            val sources = scala.collection.mutable.ListBuffer.empty[SourceSchema]
            val destinations = scala.collection.mutable.ListBuffer.empty[DestSchema]
            dataset.foreach(hlist => { sources += get(hlist, Witness('source)) ; destinations += get(hlist, Witness('dest)) })
            (source_model(sources.distinct.to(List)), dest_model(destinations.distinct.to(List)))
        }
    )
}
object GetNodes extends LowPriorityGetNodes {
    def apply[Repr <: HList, ModelOut <: Model[_]](implicit ok: GetNodes[Repr, ModelOut]): Aux[Repr, ModelOut, ok.Out] = ok

    implicit def source_schema_and_dest_schema_are_same [
        SourceSchema <: HList, DestSchema <: HList, EdgeSchema <: HList, 
        ModelOut <: Model[_]
    ](
        implicit
        convert_to_dest: SourceSchema =:= DestSchema,
        nodes_model: IsValidSchema[DestSchema, ModelOut, HNil]
    ) = inhabit_Type[
        FieldType[Witness.`'source`.T, SourceSchema] :: FieldType[Witness.`'dest`.T, DestSchema] :: FieldType[Witness.`'edge`.T, EdgeSchema] :: HNil, 
        ModelOut, 
        nodes_model.Out
    ](
        (dataset: List[FieldType[Witness.`'source`.T, SourceSchema] :: FieldType[Witness.`'dest`.T, DestSchema] :: FieldType[Witness.`'edge`.T, EdgeSchema] :: HNil]) => { 
            val nodes = scala.collection.mutable.ListBuffer.empty[DestSchema]
            dataset.foreach(hlist => nodes ++= List(convert_to_dest(get(hlist, Witness('source))), get(hlist, Witness('dest))))
            nodes_model(nodes.distinct.to(List))
        }
    )
}