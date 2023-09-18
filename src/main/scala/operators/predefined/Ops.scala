package pridwen.operators.predefined

import shapeless.{HList, HNil, ::, Witness => W}
import shapeless.labelled.{FieldType => Field}

import pridwen.models._
import pridwen.models.aux.{SelectAtt}

object ops {
    /* def constructGraph[
        M <: Model,
        Path_To_Sources <: HList,
        Path_To_Dests <: HList,
        Nodes_Id,
        Path_To_Edges <: HList,
        NS <: HList,
        ES <: HList
    ](
        data: List[M],
        sources: Path_To_Sources,
        dests: Path_To_Dests,
        nodes_id: Nodes_Id,
        edges: Path_To_Edges
    )(
        implicit
        sn1: SelectAtt.Aux[M#Schema, Path_To_Sources, NS],
        sn2: SelectAtt.Aux[M#Schema, Path_To_Dests, NS],
        sn3: SelectAtt[NS, Nodes_Id::HNil],
        se: SelectAtt.Aux[M#Schema, Path_To_Edges, ES]
    ): List[Graph[Field[W.`'source`.T, NS] :: Field[W.`'dest`.T, NS] :: Field[W.`'edge`.T, ES] :: HNil]] = ??? */
        //= data.map(hlist => )
}