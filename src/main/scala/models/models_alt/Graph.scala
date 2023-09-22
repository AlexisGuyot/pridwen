package pridwen.models.alt

import shapeless.{HList, HNil, ::, Witness, Lazy}
import shapeless.labelled.{FieldType, field}
import shapeless.ops.hlist.{Prepend}
import shapeless.ops.record.{Selector => RSelector}

import java.time.{LocalDate => Date}

import pridwen.support.{DeepGeneric}
import pridwen.support.functions.{getFieldValue}


abstract class Graph[S, SID, DID](dataset: List[S]) extends Model[S](dataset) {
    type E <: HList ; type V <: HList 
}
object Graph { 
    type Aux[S, SID, DID, E0 <: HList, V0 <: HList] = Graph[S, SID, DID] { type Repr = E0 ; type E = E0 ; type V = V0 }
    def apply[S, SID, DID](dataset: List[S])(implicit ok: ValidGraph[S, SID, DID]) = ok(dataset)
}

trait ValidGraph[S, SID, DID] { type Out <: Graph[_,_,_] ; def apply(dataset: List[S]): Out }
object ValidGraph {
    def apply[S, SID, DID](implicit ok: ValidGraph[S, SID, DID]): Aux[S, SID, DID, ok.Out] = ok
    type Aux[S, SID, DID, Out0 <: Graph[_,_,_]] = ValidGraph[S, SID, DID] { type Out = Out0 }
    protected def inhabit_Type[S, SID, DID, E0 <: HList, V0 <: HList](f: S => E0): Aux[S, SID, DID, Graph.Aux[S, SID, DID, E0, V0]] = new ValidGraph[S, SID, DID] { type Out = Graph.Aux[S, SID, DID, E0, V0] ; def apply(dataset: List[S]) = new Graph[S, SID, DID](dataset) { type Repr = E0 ; type E = E0; type V = V0 ; def toRepr(s: S) = f(s) } }

    implicit def case_class_schema[CCS <: Product, S <: HList, SID, DID, E0 <: HList, V0 <: HList](
        implicit
        gen: DeepGeneric.Aux[CCS, S],
        g: ValidGraph.Aux[S, SID, DID, Graph.Aux[S, SID, DID, E0, V0]]
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
        g: Lazy[ValidGraph.Aux[SS :: DS :: ES :: HNil, SID, DID, Graph.Aux[SS :: DS :: ES :: HNil, SID, DID, E0, V0]]]
    ) = inhabit_Type[FieldType[SK, SS] :: FieldType[DK, DS] :: FieldType[EK, ES] :: HNil, SID, DID, E0, V0](
        (s: FieldType[SK, SS] :: FieldType[DK, DS] :: FieldType[EK, ES] :: HNil) => g.value(List(getFieldValue(s.head) :: getFieldValue(s.tail.head) :: getFieldValue(s.tail.tail.head) :: HNil)).data.head
    )
}