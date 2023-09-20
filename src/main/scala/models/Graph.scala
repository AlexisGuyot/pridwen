package pridwen.models

import shapeless.{HList, HNil, ::, Witness, Lazy}
import shapeless.labelled.{FieldType, field}
import shapeless.ops.hlist.{Prepend}
import shapeless.ops.record.{Selector => RSelector}

import java.time.{LocalDate => Date}

import pridwen.support.{DeepGeneric}
import pridwen.support.functions.{getFieldValue}

trait Graph[S, VID] extends Model[S] { 
    type E <: HList ; type V <: HList 
    type Repr = E 
}
object Graph {
    type Aux[S, VID, E0 <: HList, V0 <: HList] = Graph[S, VID] { type E = E0 ; type V = V0 }
    protected def inhabit_Type[S, VID, E0 <: HList, V0 <: HList](f: S => E0): Aux[S, VID, E0, V0] = new Graph[S, VID] { type E = E0 ; type V = V0 ; def toRepr(s: S) = f(s) }
    def apply[S, VID](implicit ok: Graph[S, VID]): Aux[S, VID, ok.E, ok.V] = ok
    
    //def load[S, VID](g: Graph[S, VID])(dataset: List[g.Repr]) = new Graph[g.Repr, VID] { type E = g.E ; type V = g.V ; override val data = dataset ; def toRepr(s: g.Repr) = s }
    def load[S, VID](g: Graph[S, VID])(dataset: List[S]) = new Graph[S, VID] { type E = g.E ; type V = g.V ; override val data = dataset.map(s => toRepr(s)) ; def toRepr(s: S) = g.toRepr(s) }

    implicit def case_class_schema[CCS <: Product, S <: HList, VID, E0 <: HList, V0 <: HList](
        implicit
        gen: DeepGeneric.Aux[CCS, S],
        g: Graph.Aux[S, VID, E0, V0]
    ) = inhabit_Type[CCS, VID, E0, V0](
        (s: CCS) => g.toRepr(gen.to(s))
    )

    implicit def edge_list[SS <: HList, DS <: HList, ES <: HList, VID, NS <: HList]
    (
        implicit
        i1: Relation[SS],
        i2: Relation[DS],
        i3: Relation[ES],
        s1: RSelector[SS, VID],
        s2: RSelector[DS, VID],
        p: Prepend.Aux[SS, DS, NS]
    ) = inhabit_Type[
        SS :: DS :: ES :: HNil, VID,
        FieldType[Witness.`'source`.T, SS] :: FieldType[Witness.`'dest`.T, DS] :: FieldType[Witness.`'edge`.T, ES] :: HNil,
        FieldType[Witness.`'nodes`.T, NS] :: HNil
    ](
        (s: SS :: DS :: ES :: HNil) => field[Witness.`'source`.T](s.head) :: field[Witness.`'dest`.T](s.tail.head) :: field[Witness.`'edge`.T](s.tail.tail.head) :: HNil
    )

    implicit def edge_list_with_nested_fields[SK, DK, EK, SS <: HList, DS <: HList, ES <: HList, VID, E0 <: HList, V0 <: HList](
        implicit
        g: Lazy[Graph.Aux[SS :: DS :: ES :: HNil, VID, E0, V0]]
    ) = inhabit_Type[FieldType[SK, SS] :: FieldType[DK, DS] :: FieldType[EK, ES] :: HNil, VID, E0, V0](
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