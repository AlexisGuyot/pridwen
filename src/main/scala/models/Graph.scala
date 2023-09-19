package pridwen.models

import shapeless.{HList, HNil, ::, Witness}
import shapeless.labelled.{FieldType, field}
import shapeless.ops.record.{Selector => RSelector}

import java.time.{LocalDate => Date}

import pridwen.models.aux.{SelectAtt}
import pridwen.support.functions.{getFieldValue}

trait Graph[S <: HList, NID] extends Model { type NodeKey = NID ; def format(data: List[S]): List[Schema] }
trait SomeGraph extends Graph[HNil, Witness.`'id`.T]
trait LowPriorityGraph {
    type Aux[S <: HList, NID, Schema0 <: HList] = Graph[S, NID] { type Schema = Schema0 }
    type IsConform[S <: HList, NID] = Aux[S, NID, S]
    protected def inhabit_Type[S <: HList, NID, Schema0 <: HList](f: List[S] => List[Schema0]): Aux[S, NID, Schema0] = new Graph[S, NID] { override type Schema = Schema0 ; def format(data: List[S]) = f(data) ; val data = List() }

    type DefaultSourceName = Witness.`'source`.T
    type DefaultDestName = Witness.`'dest`.T
    type DefaultEdgeName = Witness.`'edge`.T

    implicit def edge_list2[SS <: HList, DS <: HList, ES <: HList, NID]
    (
        implicit
        //i1: Relation.IsConform[SS],
        //i2: Relation.IsConform[DS],
        i3: Relation.IsConform[ES],
        s1: RSelector[SS, NID],
        s2: RSelector[DS, NID]
    ) = inhabit_Type[
        SS :: DS :: ES :: HNil, NID,
        FieldType[DefaultSourceName, SS] :: FieldType[DefaultDestName, DS] :: FieldType[DefaultEdgeName, ES] :: HNil
    ](
        (data: List[SS :: DS :: ES :: HNil]) => data.map(hlist => field[DefaultSourceName](hlist.head) :: field[DefaultDestName](hlist.tail.head) :: field[DefaultEdgeName](hlist.tail.tail.head) :: HNil)
    )
}
object Graph extends LowPriorityGraph {
    def apply[S <: HList](d: List[S], nid: Witness)(implicit ok: Graph[S, nid.T]): Aux[S, nid.T, ok.Schema] = new Graph[S, nid.T] { override type Schema = ok.Schema ; def format(data: List[S]) = ok.format(data) ; val data = ok.format(d) }

    implicit def edge_list[SK, DK, EK, SS <: HList, DS <: HList, ES <: HList, NID]
    (
        implicit
        //i1: Relation.IsConform[SS],
        //i2: Relation.IsConform[DS],
        i3: Relation.IsConform[ES],
        s1: RSelector[SS, NID],
        s2: RSelector[DS, NID]
    ) = inhabit_Type[
        FieldType[SK, SS] :: FieldType[DK, DS] :: FieldType[EK, ES] :: HNil, NID,
        FieldType[DefaultSourceName, SS] :: FieldType[DefaultDestName, DS] :: FieldType[DefaultEdgeName, ES] :: HNil
    ](
        (data: List[FieldType[SK, SS] :: FieldType[DK, DS] :: FieldType[EK, ES] :: HNil]) => data.map(hlist => field[DefaultSourceName](getFieldValue(hlist.head)) :: field[DefaultDestName](getFieldValue(hlist.tail.head)) :: field[DefaultEdgeName](getFieldValue(hlist.tail.tail.head)) :: HNil)
    )

    implicit def node_list[NK, NS <: HList, NID](
        implicit
        i: Relation.IsConform[NS],
        s: RSelector[NS, NID]
    ) = inhabit_Type[
        FieldType[NK, NS] :: HNil, NID,
        FieldType[DefaultSourceName, NS] :: FieldType[DefaultDestName, NS] :: FieldType[DefaultEdgeName, HNil] :: HNil
    ](
        (data: List[FieldType[NK, NS] :: HNil]) => data.map(hlist => field[DefaultSourceName](getFieldValue(hlist.head)) :: field[DefaultDestName](getFieldValue(hlist.head)) :: field[DefaultEdgeName](HNil) :: HNil)
    )
}

/* trait Graph[S <: HList] extends Model
trait SomeGraph extends Graph[HNil] { type Schema = HNil ; val data = List(HNil) }
object Graph {

    type NodeKey = Witness.`'id`.T 

    type Aux[S <: HList, Schema0 <: HList] = Graph[S] { type Schema = Schema0 }
    type IsConform[S <: HList] = Aux[S, S]
    protected def inhabit_Type[S <: HList, Schema0 <: HList]: Aux[S, Schema0] = new Graph[S] { type Schema = Schema0 ; val data = List() }
    
    def apply[S <: HList](d: List[S])(implicit ok: Graph[S]): Aux[S, ok.Schema] = new Graph[S] { type Schema = ok.Schema ; val data = d }

    implicit def edge_list[SK, DK, EK, SS, DS, ES, Out0, Out1](
        implicit
        c1: CheckSchemaNodes.Aux[SS, Out0],
        c2: CheckSchemaNodes.Aux[DS, Out1],
        c3: CheckSchemaEdges[ES]
    ) = inhabit_Type[
        FieldType[SK, SS] :: FieldType[DK, DS] :: FieldType[EK, ES] :: HNil,
        FieldType[Witness.`'source`.T, Out0] :: FieldType[Witness.`'dest`.T, Out1] :: FieldType[Witness.`'edge`.T, ES] :: HNil
    ]
    
    implicit def node_list[NK, NS, Out0](
        implicit
        c: CheckSchemaNodes.Aux[NS, Out0]
    ) = inhabit_Type[
        FieldType[NK, NS] :: HNil,
        FieldType[Witness.`'nodes`.T, Out0] :: HNil
    ]
} */