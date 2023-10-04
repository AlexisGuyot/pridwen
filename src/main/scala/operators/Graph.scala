package pridwen.operators

import shapeless.{HList, ::, HNil, Witness}

import collection.mutable.Map

import pridwen.models.{Graph, Model, GetNodes}
import pridwen.models.aux.{SelectField}
import pridwen.support.functions.{getFieldValue}

object graph {
    // Idée d'amélioration : Passer ces fonctions dans le type Graph directement pour des appels du type mon_graphe.nodes ou mon_graphe.adjacency_matrix, etc.

    def nodes[S, SourceID, DestID, ModelOut <: Model[_]](
        dataset: Graph[S, SourceID, DestID],
        mout: ModelOut
    )(
        implicit
        get_nodes: GetNodes[dataset.Repr, ModelOut]
    ): get_nodes.Out = get_nodes(dataset.data)



    def adjacency_matrix [
        S, SourceID, DestID,
        SourceID_Type, DestID_Type
    ](
        dataset: Graph[S, SourceID, DestID],
        weight_att: Witness
    )(
        implicit
        get_source_id: SelectField.Aux[dataset.Repr, Witness.`'source`.T :: SourceID :: HNil, SourceID, SourceID_Type],
        get_dest_id: SelectField.Aux[dataset.Repr, Witness.`'dest`.T :: DestID :: HNil, DestID, DestID_Type],
        get_edge_weight: SelectField.Aux[dataset.Repr, Witness.`'edge`.T :: weight_att.T :: HNil, weight_att.T, Int]
    ): Map[SourceID_Type, Map[DestID_Type, Int]] = {
        var sparse_matrix: Map[SourceID_Type, Map[DestID_Type, Int]] = Map()
        dataset.data.foreach(hlist => {
            val source_id = getFieldValue(get_source_id(hlist))
            val dest_id = getFieldValue(get_dest_id(hlist))
            var nested_map = sparse_matrix.getOrElse(source_id, Map())
            var current_weight = nested_map.getOrElse(dest_id, 0)
            nested_map(dest_id) = current_weight + getFieldValue(get_edge_weight(hlist))
            sparse_matrix(source_id) = nested_map
        })
        sparse_matrix
    }



    def community_matrix [
        S, SourceID, DestID, 
        NodeID_Type, Community_Type
    ](
        dataset: Graph[S, SourceID, DestID],
        community_att: Witness
    )(
        implicit
        get_source_id: SelectField.Aux[dataset.Repr, Witness.`'source`.T :: SourceID :: HNil, SourceID, NodeID_Type],
        get_dest_id: SelectField.Aux[dataset.Repr, Witness.`'dest`.T :: DestID :: HNil, DestID, NodeID_Type],
        get_source_community: SelectField.Aux[dataset.Repr, Witness.`'source`.T :: community_att.T :: HNil, community_att.T, Community_Type],
        get_dest_community: SelectField.Aux[dataset.Repr, Witness.`'dest`.T :: community_att.T :: HNil, community_att.T, Community_Type],
    ): Map[NodeID_Type, Map[Community_Type, Boolean]] = {
        var sparse_matrix: Map[NodeID_Type, Map[Community_Type, Boolean]] = Map()
        dataset.data.foreach(hlist => {
            val source_id = getFieldValue(get_source_id(hlist))
            val dest_id = getFieldValue(get_dest_id(hlist))
            if(!sparse_matrix.contains(source_id)) sparse_matrix(source_id) = Map((getFieldValue(get_source_community(hlist)), true))
            if(!sparse_matrix.contains(dest_id)) sparse_matrix(dest_id) = Map((getFieldValue(get_dest_community(hlist)), true))
        })
        sparse_matrix
    }
}