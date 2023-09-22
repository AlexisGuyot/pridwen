package pridwen.models

import shapeless.{HList, HNil, ::, Witness, Lazy}
import shapeless.labelled.{FieldType, field}
import shapeless.ops.hlist.{Prepend}
import shapeless.ops.record.{Selector => RSelector}

import java.time.{LocalDate => Date}

import pridwen.support.{DeepGeneric}
import pridwen.support.functions.{getFieldValue}

abstract class Graph[S, SID, DID](dataset: List[S]) extends Model[S](dataset) { 
    type E <: HList ; type V <: HList 
    type Repr = E 
}
object Graph {
    type Aux[S, SID, DID, E0 <: HList, V0 <: HList] = Graph[S, SID, DID] { type E = E0 ; type V = V0 }
    protected def inhabit_Type[S, SID, DID, E0 <: HList, V0 <: HList](f: S => E0): Aux[S, SID, DID, E0, V0] = new Graph[S, SID, DID](List()) { type E = E0 ; type V = V0 ; def toRepr(s: S) = f(s) }
    def apply[S, SID, DID](dataset: List[S])(implicit ok: Graph[S, SID, DID]): Aux[S, SID, DID, ok.E, ok.V] = new Graph[S, SID, DID](dataset) { type E = ok.E ; type V = ok.V ; def toRepr(s: S) = ok.toRepr(s) }
    
    //def load[S, VID](g: Graph[S, VID])(dataset: List[g.Repr]) = new Graph[g.Repr, VID] { type E = g.E ; type V = g.V ; override val data = dataset ; def toRepr(s: g.Repr) = s }
    def load[S, SID, DID](g: Graph[S, SID, DID])(dataset: List[S]) = new Graph[S, SID, DID](List()) { type E = g.E ; type V = g.V ; override val data = dataset.map(s => toRepr(s)) ; def toRepr(s: S) = g.toRepr(s) }

    implicit def case_class_schema[CCS <: Product, S <: HList, SID, DID, E0 <: HList, V0 <: HList](
        implicit
        gen: DeepGeneric.Aux[CCS, S],
        g: Graph.Aux[S, SID, DID, E0, V0]
    ) = inhabit_Type[CCS, SID, DID, E0, V0](
        (s: CCS) => g.toRepr(gen.to(s))
    )

    implicit def edge_list[SS <: HList, DS <: HList, ES <: HList, SID, DID, NS <: HList]
    (
        implicit
        i1: Relation[SS],
        i2: Relation[DS],
        i3: Relation[ES],
        s1: RSelector[SS, SID],
        s2: RSelector[DS, DID],
        p: Prepend.Aux[SS, DS, NS]
    ) = inhabit_Type[
        SS :: DS :: ES :: HNil, SID, DID,
        FieldType[Witness.`'source`.T, SS] :: FieldType[Witness.`'dest`.T, DS] :: FieldType[Witness.`'edge`.T, ES] :: HNil,
        FieldType[Witness.`'nodes`.T, NS] :: HNil
    ](
        (s: SS :: DS :: ES :: HNil) => field[Witness.`'source`.T](s.head) :: field[Witness.`'dest`.T](s.tail.head) :: field[Witness.`'edge`.T](s.tail.tail.head) :: HNil
    )

    implicit def edge_list_with_nested_fields[SK, DK, EK, SS <: HList, DS <: HList, ES <: HList, SID, DID, E0 <: HList, V0 <: HList](
        implicit
        g: Lazy[Graph.Aux[SS :: DS :: ES :: HNil, SID, DID, E0, V0]]
    ) = inhabit_Type[FieldType[SK, SS] :: FieldType[DK, DS] :: FieldType[EK, ES] :: HNil, SID, DID, E0, V0](
        (s: FieldType[SK, SS] :: FieldType[DK, DS] :: FieldType[EK, ES] :: HNil) => g.value.toRepr(getFieldValue(s.head) :: getFieldValue(s.tail.head) :: getFieldValue(s.tail.tail.head) :: HNil)
    )

    /* implicit def node_list[NK, NS <: HList, VID](
        implicit
        i: Relation[NS],
        s: RSelector[NS, VID]
    ) = inhabit_Type[
        FieldType[NK, NS] :: HNil, VID,
        HNil,
        FieldType[DefaultNodeName, NS] :: HNil
    ] */
}