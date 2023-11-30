package pridwen.models

import shapeless.{HList, ::, HNil, Witness}
import shapeless.labelled.{FieldType => Field, field}

import java.time.{LocalDate => Date}
import scala.collection.parallel.CollectionConverters._

import pridwen.support.{DeepGeneric}
import pridwen.support.functions.{getFieldValue}
import pridwen.schemaop.{SelectField}

trait Graph extends Model { 
    type SourceID ; type DestID 
    type SourceSchema <: HList ; type DestSchema <: HList

    protected class NodeReturn[T](d: List[T]) {
        def apply: List[T] = asList
        def asList: List[T] = d
        def as[ModelOut <: Model](implicit new_dataset: Model.As[T, ModelOut]): new_dataset.T = new_dataset(d)
    }

    def nodes(implicit eq: =:=[SourceSchema, DestSchema]): NodeReturn[SourceSchema]
    def source_nodes: NodeReturn[SourceSchema]
    def dest_nodes: NodeReturn[DestSchema]
}

object Graph {
    type SourceName = Witness.`'source`.T ; type DestName = Witness.`'dest`.T ; type EdgeName = Witness.`'edge`.T

    type Aux[S <: HList, SID, DID, SS <: HList, DS <: HList] = Graph { type Schema = S ; type SourceID = SID ; type DestID = DID ; type SourceSchema = SS ; type DestSchema = DS }
    type WithID[SID, DID] = Aux[HNil, SID, DID, HNil, HNil]

    def apply[Schema, SID, DID](dataset: List[Schema])(implicit isValid: ValidSchema[Schema, SID, DID]): isValid.T = isValid(dataset)
    def apply[Schema](dataset: List[Schema], sourceID: Witness, destID: Witness)(implicit isValid: ValidSchema[Schema, sourceID.T, destID.T]): isValid.T = isValid(dataset)


    trait ValidSchema[S, SourceID, DestID] { type Repr <: HList ; type SRepr <: HList ; type DRepr <: HList ; type T ; def apply(dataset: List[S]): T ; val toRepr: S => Repr ; val toSourceRepr: S => SRepr ; val toDestRepr: S => DRepr }
    object ValidSchema {
        type Aux[S, SourceID, DestID, Repr0 <: HList, SRepr0 <: HList, DRepr0 <: HList, T0] = ValidSchema[S, SourceID, DestID] { type Repr = Repr0 ; type SRepr = SRepr0 ; type DRepr = DRepr0 ; type T = T0 }

        protected def inhabit_Type[S, SID, DID, Repr0 <: HList, SRepr0 <: HList, DRepr0 <: HList](
            toRepr0: S => Repr0,
            toSourceRepr0: S => SRepr0,
            toDestRepr0: S => DRepr0
        ): Aux[S, SID, DID, Repr0, SRepr0, DRepr0, Graph.Aux[Repr0, SID, DID, SRepr0, DRepr0]]
            = new ValidSchema[S, SID, DID] {
                type Repr = Repr0 ; type SRepr = SRepr0 ; type DRepr = DRepr0
                type T = Graph.Aux[Repr0, SID, DID, SRepr0, DRepr0]
                val toRepr = toRepr0 ; val toSourceRepr = toSourceRepr0 ; val toDestRepr = toDestRepr0
                def apply(dataset: List[S]): Graph.Aux[Repr, SID, DID, SRepr, DRepr] = new Graph {
                    type Schema = Repr ; type SourceSchema = SRepr ; type DestSchema = DRepr
                    type SourceID = SID ; type DestID = DID
                    
                    val data: List[Schema] = convert_to_repr(dataset, toRepr)

                    def nodes(implicit eq: =:=[SourceSchema, DestSchema]): NodeReturn[SourceSchema] = new NodeReturn[SourceSchema](dataset.par.flatMap(schema => List[SourceSchema](toSourceRepr(schema), eq.flip(toDestRepr(schema)))).distinct.toList)
                    def source_nodes: NodeReturn[SourceSchema] = new NodeReturn[SourceSchema](dataset.par.map(schema => toSourceRepr(schema)).distinct.toList)
                    def dest_nodes: NodeReturn[DestSchema] = new NodeReturn[DestSchema](dataset.par.map(schema => toDestRepr(schema)).distinct.toList)
                }
            }

        implicit def edge_list_with_edge_attributes[HS, TS <: HList, HD, TD <: HList, HE, TE <: HList, SourceID, DestID]
        (
            implicit
            get_sourceID: SelectField[(HS::TS), SourceID],
            get_destID: SelectField[(HD::TD), DestID],
        ) = inhabit_Type[
            (HS::TS) :: (HD::TD) :: (HE::TE) :: HNil, SourceID, DestID,
            Field[Graph.SourceName, (HS::TS)] :: Field[Graph.DestName, (HD::TD)] :: Field[Graph.EdgeName, (HE::TE)] :: HNil,
            (HS::TS), (HD::TD)
        ](
            (schema: (HS::TS) :: (HD::TD) :: (HE::TE) :: HNil) => field[Graph.SourceName](schema.head) :: field[Graph.DestName](schema.tail.head) :: field[Graph.EdgeName](schema.tail.tail.head) :: HNil,
            (schema: (HS::TS) :: (HD::TD) :: (HE::TE) :: HNil) => schema.head,
            (schema: (HS::TS) :: (HD::TD) :: (HE::TE) :: HNil) => schema.tail.head
        )

        implicit def edge_list_without_edge_attributes[HS, TS <: HList, HD, TD <: HList, SourceID, DestID]
        (
            implicit
            get_sourceID: SelectField[(HS::TS), SourceID],
            get_destID: SelectField[(HD::TD), DestID],
        ) = inhabit_Type[
            (HS::TS) :: (HD::TD) :: HNil, SourceID, DestID,
            Field[Graph.SourceName, (HS::TS)] :: Field[Graph.DestName, (HD::TD)] :: Field[Graph.EdgeName, HNil] :: HNil,
            (HS::TS), (HD::TD)
        ](
            (schema: (HS::TS) :: (HD::TD) :: HNil) => field[Graph.SourceName](schema.head) :: field[Graph.DestName](schema.tail.head) :: field[Graph.EdgeName](HNil) :: HNil,
            (schema: (HS::TS) :: (HD::TD) :: HNil) => schema.head,
            (schema: (HS::TS) :: (HD::TD) :: HNil) => schema.tail.head
        )

        implicit def nested_edge_list_with_edge_attributes[SourceSchema <: HList, SourceKey, DestSchema <: HList, DestKey, EdgeSchema <: HList, EdgeKey, SourceID, DestID]
        (
            implicit
            get_sourceID: SelectField[SourceSchema, SourceID],
            get_destID: SelectField[DestSchema, DestID],
        ) = inhabit_Type[
            Field[SourceKey, SourceSchema] :: Field[DestKey, DestSchema] :: Field[EdgeKey, EdgeSchema] :: HNil, SourceID, DestID,
            Field[Graph.SourceName, SourceSchema] :: Field[Graph.DestName, DestSchema] :: Field[Graph.EdgeName, EdgeSchema] :: HNil,
            SourceSchema, DestSchema
        ](
            (schema: Field[SourceKey, SourceSchema] :: Field[DestKey, DestSchema] :: Field[EdgeKey, EdgeSchema] :: HNil) => field[Graph.SourceName](getFieldValue(schema.head)) :: field[Graph.DestName](getFieldValue(schema.tail.head)) :: field[Graph.EdgeName](getFieldValue(schema.tail.tail.head)) :: HNil,
            (schema: Field[SourceKey, SourceSchema] :: Field[DestKey, DestSchema] :: Field[EdgeKey, EdgeSchema] :: HNil) => schema.head,
            (schema: Field[SourceKey, SourceSchema] :: Field[DestKey, DestSchema] :: Field[EdgeKey, EdgeSchema] :: HNil) => schema.tail.head
        )

        implicit def nested_edge_list_without_edge_attributes[SourceSchema <: HList, SourceKey, DestSchema <: HList, DestKey, SourceID, DestID]
        (
            implicit
            get_sourceID: SelectField[SourceSchema, SourceID],
            get_destID: SelectField[DestSchema, DestID],
        ) = inhabit_Type[
            Field[SourceKey, SourceSchema] :: Field[DestKey, DestSchema] :: HNil, SourceID, DestID,
            Field[Graph.SourceName, SourceSchema] :: Field[Graph.DestName, DestSchema] :: Field[Graph.EdgeName, HNil] :: HNil,
            SourceSchema, DestSchema
        ](
            (schema: Field[SourceKey, SourceSchema] :: Field[DestKey, DestSchema] :: HNil) => field[Graph.SourceName](getFieldValue(schema.head)) :: field[Graph.DestName](getFieldValue(schema.tail.head)) :: field[Graph.EdgeName](HNil) :: HNil,
            (schema: Field[SourceKey, SourceSchema] :: Field[DestKey, DestSchema] :: HNil) => schema.head,
            (schema: Field[SourceKey, SourceSchema] :: Field[DestKey, DestSchema] :: HNil) => schema.tail.head
        )
    }
}