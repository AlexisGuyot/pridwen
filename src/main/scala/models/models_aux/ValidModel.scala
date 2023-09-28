package pridwen.models.aux

import shapeless.{HList, Lazy}

import pridwen.models._

trait ValidModel[M <: Model[_], S, -P] { type Out ; def apply(dataset: List[S]): Out }
object ValidModel {
    def apply[M <: Model[_], S, P](implicit ok: ValidModel[M, S, P]): Aux[M, S, P, ok.Out] = ok
    type Aux[M <: Model[_], S, P, Out0] = ValidModel[M, S, P] { type Out = Out0 }
    protected def inhabit_Type[M <: Model[_], S, P, Out0](f: List[S] => Out0): Aux[M, S, P, Out0] = new ValidModel[M, S, P] { type Out = Out0 ; def apply(dataset: List[S]) = f(dataset) }

    implicit def m_is_json[M <: JSON[_], S, P, Out0 <: HList](
        implicit
        m: ValidJSON.Aux[S, Out0]
    ) = inhabit_Type[M, S, P, JSON.Aux[S, Out0]]((dataset: List[S]) => m(dataset))

    implicit def m_is_rel[M <: Relation[_], S, P, Out0 <: HList](
        implicit
        m: ValidRelation.Aux[S, Out0]
    ) = inhabit_Type[M, S, P, Relation.Aux[S, Out0]]((dataset: List[S]) => m(dataset))

    implicit def m_is_graph[M <: Graph[_,_,_], S, SID, DID, E0 <: HList, V0 <: HList](
        implicit
        m: ValidGraph.Aux[S, SID, DID, E0, V0]
    ) = inhabit_Type[M, S, (SID, DID), Graph.Aux[S, SID, DID, E0, V0]]((dataset: List[S]) => m(dataset))
}