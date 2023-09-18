/* package pridwen.models.aux

import shapeless.{HList, HNil, ::}
import shapeless.ops.hlist.{Selector => HSelector, Remove}
import shapeless.ops.record.{Selector => RSelector}
import shapeless.labelled.{FieldType}

import pridwen.models.{Relation, Graph}

trait CheckSchemaNodes[S] { type Out }
trait LowerPriorityCheckSchemaNodes {
    type Aux[S, Out0] = CheckSchemaNodes[S] { type Out = Out0 }
    protected def inhabit_Type[S, Out0]: Aux[S, Out0] = new CheckSchemaNodes[S] { type Out = Out0 }
}
trait LowPriorityCheckSchemaNodes extends LowerPriorityCheckSchemaNodes {
    implicit def rule1[S <: HList](
        implicit
        i: Relation.IsConform[S],
        s: RSelector[S, Graph.NodeKey]
    ) = inhabit_Type[S, S]
}
object CheckSchemaNodes extends LowPriorityCheckSchemaNodes {
    def apply[S](implicit ok: CheckSchemaNodes[S]): Aux[S, ok.Out] = ok

    implicit def rule2[S <: HList, NewS <: HList](
        implicit
        s1: HSelector[S, HList],
        r: Remove.Aux[S, HList, (HList, NewS)],
        i: Relation.IsConform[NewS],
    ) = inhabit_Type[S, S]

    implicit def rule3 = inhabit_Type[HList, HList]
    implicit def rule4[S1 <: HList, S2 <: HList] = inhabit_Type[(S1, S2)::HNil, (S1, S2)::HNil]

    implicit def rule5[K, V, Out0](
        implicit
        c: CheckSchemaNodes.Aux[V, Out0]
    ) = inhabit_Type[FieldType[K,V]::HNil, Out0]
}

trait CheckSchemaEdges[S]
trait LowPriorityCheckSchemaEdges {
    protected def inhabit_Type[S]: CheckSchemaEdges[S] = new CheckSchemaEdges[S] { }

    implicit def rule1[S <: HList](
        implicit
        i: Relation.IsConform[S]
    ) = inhabit_Type[S]
}
object CheckSchemaEdges extends LowPriorityCheckSchemaEdges {
    def apply[S](implicit ok: CheckSchemaEdges[S]): CheckSchemaEdges[S] = ok

    implicit def rule2[S <: HList, NewS <: HList](
        implicit
        s1: HSelector[S, HList],
        r: Remove.Aux[S, HList, (HList, NewS)],
        i: Relation.IsConform[NewS]
    ) = inhabit_Type[S]

    implicit def rule3 = inhabit_Type[HList]
} */