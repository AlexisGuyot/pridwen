package pridwen.models

import shapeless.{HList, HNil, ::, Witness}
import shapeless.labelled.{FieldType, field}
import shapeless.ops.hlist.{Prepend}

import pridwen.support.{DeepGeneric, RSelector}
import pridwen.support.functions.{getFieldValue, get}
import pridwen.models.aux.{IsValidSchema}

abstract class Graph[Schema, SourceID, DestID](dataset: List[Schema]) extends Model[Schema](dataset)
object Graph { 
    type Aux[Schema, SourceID, DestID, Repr0 <: HList] = Graph[Schema, SourceID, DestID] { type Repr = Repr0 }
    def apply[Schema, SourceID, DestID](dataset: List[Schema])(implicit ok: IsValidGraph[Schema, SourceID, DestID]) = ok(dataset)
}

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
        CSchema <: Product, HSchema <: HList, SourceID, DestID, 
        Repr0 <: HList
    ](
        implicit
        convert: DeepGeneric.Aux[CSchema, HSchema],
        graph: IsValidGraph.Aux[HSchema, SourceID, DestID, Repr0]
    ) = inhabit_Type[CSchema, SourceID, DestID, Repr0](
        (schema: CSchema) => graph(List(convert.to(schema))).data.head
    )

    implicit def edge_list[SourceSchema <: HList, DestSchema <: HList, ES <: HList, SourceID, DestID, NS <: HList]
    (
        implicit
        i1: ValidRelation[SourceSchema],
        i2: ValidRelation[DestSchema],
        i3: ValidRelation[ES],
        s1: RSelector[SourceSchema, SourceID],
        s2: RSelector[DestSchema, DestID],
        p: Prepend.Aux[SourceSchema, DestSchema, NS]
    ) = inhabit_Type[
        SourceSchema :: DestSchema :: ES :: HNil, SourceID, DestID,
        FieldType[Witness.`'source`.T, SourceSchema] :: FieldType[Witness.`'dest`.T, DestSchema] :: FieldType[Witness.`'edge`.T, ES] :: HNil
    ](
        (s: SourceSchema :: DestSchema :: ES :: HNil) => field[Witness.`'source`.T](s.head) :: field[Witness.`'dest`.T](s.tail.head) :: field[Witness.`'edge`.T](s.tail.tail.head) :: HNil
    )

    implicit def edge_list_with_nested_fields[SK, DK, EK, SourceSchema <: HList, DestSchema <: HList, ES <: HList, NS <: HList, SourceID, DestID, Repr0 <: HList](
        implicit
        //g: IsValidGraph.Aux[SourceSchema :: DestSchema :: ES :: HNil, SourceID, DestID, E0, V0]
        i1: ValidRelation[SourceSchema],
        i2: ValidRelation[DestSchema],
        i3: ValidRelation[ES],
        s1: RSelector[SourceSchema, SourceID],
        s2: RSelector[DestSchema, DestID],
        p: Prepend.Aux[SourceSchema, DestSchema, NS]
    ) = inhabit_Type[
        FieldType[SK, SourceSchema] :: FieldType[DK, DestSchema] :: FieldType[EK, ES] :: HNil, SourceID, DestID, 
        FieldType[Witness.`'source`.T, SourceSchema] :: FieldType[Witness.`'dest`.T, DestSchema] :: FieldType[Witness.`'edge`.T, ES] :: HNil
    ](
        (s: FieldType[SK, SourceSchema] :: FieldType[DK, DestSchema] :: FieldType[EK, ES] :: HNil) => field[Witness.`'source`.T](getFieldValue(s.head)) :: field[Witness.`'dest`.T](getFieldValue(s.tail.head)) :: field[Witness.`'edge`.T](getFieldValue(s.tail.tail.head)) :: HNil
    )
}

trait GetNodes[Repr <: HList, MN <: Model[_]] { type Out ; def apply(r: List[Repr]): Out }
trait LowPriorityGetNodes {
    type Aux[Repr <: HList, MN <: Model[_], Out0] = GetNodes[Repr, MN] { type Out = Out0 }
    protected def inhabit_Type[Repr <: HList, MN <: Model[_], Out0](f: List[Repr] => Out0): Aux[Repr, MN, Out0] = new GetNodes[Repr, MN] { type Out = Out0 ; def apply(r: List[Repr]) = f(r) }

    implicit def source_schema_and_dest_schema_are_different[SourceSchema <: HList, DestSchema <: HList, ES <: HList, MN <: Model[_]](
        implicit
        m1: IsValidSchema[SourceSchema, MN, HNil],
        m2: IsValidSchema[DestSchema, MN, HNil]
    ) = inhabit_Type[FieldType[Witness.`'source`.T, SourceSchema] :: FieldType[Witness.`'dest`.T, DestSchema] :: FieldType[Witness.`'edge`.T, ES] :: HNil, MN, (m1.Out, m2.Out)](
        (r: List[FieldType[Witness.`'source`.T, SourceSchema] :: FieldType[Witness.`'dest`.T, DestSchema] :: FieldType[Witness.`'edge`.T, ES] :: HNil]) => {
            val d1 = scala.collection.mutable.ListBuffer.empty[SourceSchema]
            val d2 = scala.collection.mutable.ListBuffer.empty[DestSchema]
            r.foreach(hlist => { d1 += get(hlist, Witness('source)) ; d2 += get(hlist, Witness('dest)) })
            (m1(d1.distinct.to(List)), m2(d2.distinct.to(List)))
        }
    )
}
object GetNodes extends LowPriorityGetNodes {
    def apply[Repr <: HList, MN <: Model[_]](implicit ok: GetNodes[Repr, MN]): Aux[Repr, MN, ok.Out] = ok

    implicit def source_schema_and_dest_schema_are_same[SourceSchema <: HList, DestSchema <: HList, ES <: HList, MN <: Model[_]](
        implicit
        eq: SourceSchema =:= DestSchema,
        m: IsValidSchema[DestSchema, MN, HNil]
    ) = inhabit_Type[FieldType[Witness.`'source`.T, SourceSchema] :: FieldType[Witness.`'dest`.T, DestSchema] :: FieldType[Witness.`'edge`.T, ES] :: HNil, MN, m.Out](
        (r: List[FieldType[Witness.`'source`.T, SourceSchema] :: FieldType[Witness.`'dest`.T, DestSchema] :: FieldType[Witness.`'edge`.T, ES] :: HNil]) => { 
            val d = scala.collection.mutable.ListBuffer.empty[DestSchema]
            r.foreach(hlist => d ++= List(eq(get(hlist, Witness('source))), get(hlist, Witness('dest))))
            m(d.distinct.to(List))
        }
    )
}