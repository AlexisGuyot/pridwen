/* package pridwen.models

import shapeless.{HList, HNil, ::, Witness}
import shapeless.labelled.{FieldType}

import java.time.{LocalDate => Date}

import pridwen.models.aux.{CheckSchemaNodes, CheckSchemaEdges}

trait Graph[S <: HList] extends Model
trait SomeGraph extends Graph[HNil] { type Schema = HNil }
object Graph {
    type NodeKey = Witness.`'id`.T 

    type Aux[S <: HList, Schema0 <: HList] = Graph[S] { type Schema = Schema0 }
    type IsConform[S <: HList] = Aux[S, S]
    protected def inhabit_Type[S <: HList, Schema0 <: HList]: Aux[S, Schema0] = new Graph[S] { type Schema = Schema0 }
    
    def apply[S <: HList](implicit ok: Graph[S]): Aux[S, ok.Schema] = ok

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