package pridwen.models

import shapeless.{HList, HNil}

trait Model[S] { 
    type Repr <: HList
    val data: List[Repr] = List() 
    def toRepr(s: S): Repr
}
object Model {
    type Aux[M[_] <: Model[_], S, Repr0 <: HList] = M[S] { type Repr = Repr0 }
}

trait GetModelRepr[M] { type Out <: HList }
object GetModelRepr {
    def apply[M](implicit ok: GetModelRepr[M]): Aux[M, ok.Out] = ok
    type Aux[M, Out0 <: HList] = GetModelRepr[M] { type Out = Out0 }
    private def inhabit_Type[M, Out0 <: HList]: Aux[M, Out0] = new GetModelRepr[M] { type Out = Out0 }

    implicit def get_aux_json[S, M[_] <: JSON[_], Repr0 <: HList](
        implicit
        j: JSON.Aux[S, Repr0]
    ) = inhabit_Type[M[S], Repr0]

    implicit def get_aux_relation[S, M[_] <: Relation[_], Repr0 <: HList](
        implicit
        r: Relation.Aux[S, Repr0]
    ) = inhabit_Type[M[S], Repr0]

    implicit def get_aux_graph[S, VID, M[_, _] <: Graph[_, _], E0 <: HList, V0 <: HList](
        implicit
        g: Graph.Aux[S, VID, E0, V0]
    ) = inhabit_Type[M[S, VID], E0]
}