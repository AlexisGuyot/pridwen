package pridwen.models

import shapeless.{HList, HNil, ::, Witness, Lazy}
import shapeless.labelled.{FieldType, field}
import shapeless.ops.hlist.{Prepend}
import shapeless.ops.record.{Selector => RSelector}

import java.time.{LocalDate => Date}

import pridwen.support.{DeepGeneric}
import pridwen.support.functions.{getFieldValue, get}
import pridwen.models.aux.{ValidModel}


abstract class Graph[S, SID, DID](dataset: List[S]) extends Model[S](dataset) {
    type E <: HList ; type V <: HList 
}
object Graph { 
    type Aux[S, SID, DID, E0 <: HList, V0 <: HList] = Graph[S, SID, DID] { type Repr = E0 ; type E = E0 ; type V = V0 }
    def apply[S, SID, DID](dataset: List[S])(implicit ok: ValidGraph[S, SID, DID]) = ok(dataset)
}

trait ValidGraph[S, SID, DID] { type E <: HList ; type V <: HList ; def apply(dataset: List[S]): Graph.Aux[S, SID, DID, E, V] }
trait LowPriorityValidGraph {
    type Aux[S, SID, DID, E0 <: HList, V0 <: HList] = ValidGraph[S, SID, DID] { type E = E0 ; type V = V0 }
    protected def inhabit_Type[S, SID, DID, E0 <: HList, V0 <: HList](f: S => E0): Aux[S, SID, DID, E0, V0] = new ValidGraph[S, SID, DID] { type E = E0 ; type V = V0 ; def apply(dataset: List[S]) = new Graph[S, SID, DID](dataset) { type Repr = E0 ; type E = E0; type V = V0 ; def toRepr(s: S) = f(s) } }

    implicit def edge_list2[SS <: HList, DS <: HList, SID, DID, NS <: HList]
    (
        implicit
        i1: ValidRelation[SS],
        i2: ValidRelation[DS],
        s1: RSelector[SS, SID],
        s2: RSelector[DS, DID],
        p: Prepend.Aux[SS, DS, NS]
    ) = inhabit_Type[
        SS :: DS :: HNil, SID, DID,
        FieldType[Witness.`'source`.T, SS] :: FieldType[Witness.`'dest`.T, DS] :: FieldType[Witness.`'edge`.T, HNil] :: HNil,
        FieldType[Witness.`'nodes`.T, NS] :: HNil
    ](
        (s: SS :: DS :: HNil) => field[Witness.`'source`.T](s.head) :: field[Witness.`'dest`.T](s.tail.head) :: field[Witness.`'edge`.T](HNil) :: HNil
    )
}
object ValidGraph extends LowPriorityValidGraph {
    def apply[S, SID, DID](implicit ok: ValidGraph[S, SID, DID]): Aux[S, SID, DID, ok.E, ok.V] = ok

    implicit def case_class_schema[CCS <: Product, S <: HList, SID, DID, E0 <: HList, V0 <: HList](
        implicit
        gen: DeepGeneric.Aux[CCS, S],
        g: ValidGraph.Aux[S, SID, DID, E0, V0]
    ) = inhabit_Type[CCS, SID, DID, E0, V0](
        (s: CCS) => g(List(gen.to(s))).data.head
    )

    implicit def edge_list[SS <: HList, DS <: HList, ES <: HList, SID, DID, NS <: HList]
    (
        implicit
        i1: ValidRelation[SS],
        i2: ValidRelation[DS],
        i3: ValidRelation[ES],
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
        g: ValidGraph.Aux[SS :: DS :: ES :: HNil, SID, DID, E0, V0]
    ) = inhabit_Type[FieldType[SK, SS] :: FieldType[DK, DS] :: FieldType[EK, ES] :: HNil, SID, DID, E0, V0](
        (s: FieldType[SK, SS] :: FieldType[DK, DS] :: FieldType[EK, ES] :: HNil) => g(List(getFieldValue(s.head) :: getFieldValue(s.tail.head) :: getFieldValue(s.tail.tail.head) :: HNil)).data.head
    )
}

trait GetNodes[Repr <: HList, MN <: Model[_]] { type Out ; def apply(r: List[Repr]): Out }
trait LowPriorityGetNodes {
    type Aux[Repr <: HList, MN <: Model[_], Out0] = GetNodes[Repr, MN] { type Out = Out0 }
    protected def inhabit_Type[Repr <: HList, MN <: Model[_], Out0](f: List[Repr] => Out0): Aux[Repr, MN, Out0] = new GetNodes[Repr, MN] { type Out = Out0 ; def apply(r: List[Repr]) = f(r) }

    implicit def source_schema_and_dest_schema_are_different[SS <: HList, DS <: HList, ES <: HList, MN <: Model[_]](
        implicit
        m1: ValidModel[MN, SS, HNil],
        m2: ValidModel[MN, DS, HNil]
    ) = inhabit_Type[FieldType[Witness.`'source`.T, SS] :: FieldType[Witness.`'dest`.T, DS] :: FieldType[Witness.`'edge`.T, ES] :: HNil, MN, (m1.Out, m2.Out)](
        (r: List[FieldType[Witness.`'source`.T, SS] :: FieldType[Witness.`'dest`.T, DS] :: FieldType[Witness.`'edge`.T, ES] :: HNil]) => {
            val d1 = scala.collection.mutable.ListBuffer.empty[SS]
            val d2 = scala.collection.mutable.ListBuffer.empty[DS]
            r.foreach(hlist => { d1 += get(hlist, Witness('source)) ; d2 += get(hlist, Witness('dest)) })
            (m1(d1.distinct.to(List)), m2(d2.distinct.to(List)))
        }
    )
}
object GetNodes extends LowPriorityGetNodes {
    def apply[Repr <: HList, MN <: Model[_]](implicit ok: GetNodes[Repr, MN]): Aux[Repr, MN, ok.Out] = ok

    implicit def source_schema_and_dest_schema_are_same[SS <: HList, DS <: HList, ES <: HList, MN <: Model[_]](
        implicit
        eq: SS =:= DS,
        m: ValidModel[MN, DS, HNil]
    ) = inhabit_Type[FieldType[Witness.`'source`.T, SS] :: FieldType[Witness.`'dest`.T, DS] :: FieldType[Witness.`'edge`.T, ES] :: HNil, MN, m.Out](
        (r: List[FieldType[Witness.`'source`.T, SS] :: FieldType[Witness.`'dest`.T, DS] :: FieldType[Witness.`'edge`.T, ES] :: HNil]) => { 
            val d = scala.collection.mutable.ListBuffer.empty[DS]
            r.foreach(hlist => d ++= List(eq(get(hlist, Witness('source))), get(hlist, Witness('dest))))
            m(d.distinct.to(List))
        }
    )
}