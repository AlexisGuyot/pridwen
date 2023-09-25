package pridwen.models.aux

import shapeless.{HNil, HList}

import pridwen.models._

trait Decompose[M] { type S ; type P }
object Decompose {
    def apply[M](implicit ok: Decompose[M]): Aux[M, ok.S, ok.P] = ok
    type Aux[M, S0, P0] = Decompose[M] { type S = S0 ; type P = P0 }
    private def inhabit_Type[M, S0, P0]: Aux[M, S0, P0] = new Decompose[M] { type S = S0 ; type P = P0 }

    implicit def decompose_json[S, Repr <: HList] = inhabit_Type[JSON.Aux[S, Repr], S, HNil]
    implicit def decompose_relation[M, S, Repr <: HList](
        implicit
        ok: M =:= Relation[S]
    ) = inhabit_Type[M, HNil, HNil]
    implicit def decompose_graph[S, SID, DID] = inhabit_Type[Graph[S, SID, DID], S, (SID, DID)]
} 