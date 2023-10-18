import shapeless.{HList, Witness, ::, HNil}
import shapeless.labelled.{field}

import pridwen.models.{Graph, IsValidGraph, GetNodes, Model, Relation}
import pridwen.models.aux.{SelectField, AddField}
import pridwen.support.functions.{get}

import org.gephi.project.api.{ProjectController, Workspace}
import org.gephi.graph.api.{GraphController, GraphModel, DirectedGraph}
import org.gephi.statistics.plugin.{Modularity}
import org.openide.util.Lookup

import scala.collection.mutable.{HashMap, HashSet}

object community_detection {
    def louvain[Schema, SourceID, DestID, SourceSchema <: HList, DestSchema <: HList, EdgeSchema <: HList, Out0 <: HList, New_Schema <: HList](
        graph_in: Graph[Schema, SourceID, DestID], 
        weight_att: Witness
    )(
        implicit
        select_sourceID: SelectField[graph_in.Repr, Witness.`'source`.T :: SourceID :: HNil],
        select_destID: SelectField[graph_in.Repr, Witness.`'dest`.T :: DestID :: HNil],
        select_weight: SelectField.Aux[graph_in.Repr, Witness.`'edge`.T :: weight_att.T :: HNil, weight_att.T, Int],
        addTo_source: AddField.Aux[graph_in.Repr, Witness.`'source`.T :: HNil, Witness.`'community`.T, Int, Out0],
        addTo_dest: AddField.Aux[Out0, Witness.`'dest`.T :: HNil, Witness.`'community`.T, Int, New_Schema],
    ): List[New_Schema] = {

        // Init Gephi Toolkit
        val pc: ProjectController = Lookup.getDefault().lookup(classOf[ProjectController])
        pc.newProject()
        val workspace: Workspace = pc.getCurrentWorkspace()
        val graphModel: GraphModel = Lookup.getDefault().lookup(classOf[GraphController]).getGraphModel()

        // Gephi Graph Construction
        val gephi_graph: DirectedGraph = graphModel.getDirectedGraph()
        graph_in.data.foreach(hlist => {
            val sourceID = select_sourceID(hlist).toString
            val destID = select_destID(hlist).toString
            val tmp_source = Option(gephi_graph.getNode(sourceID))
            val source = tmp_source getOrElse { val tmp = graphModel.factory().newNode(sourceID) ; gephi_graph.addNode(tmp) ; tmp }
            val tmp_dest = Option(gephi_graph.getNode(destID))
            val dest = tmp_dest getOrElse { val tmp = graphModel.factory().newNode(destID) ; gephi_graph.addNode(tmp) ; tmp }
            val edge = graphModel.factory().newEdge(source, dest, select_weight(hlist), true)
            gephi_graph.addEdge(edge)
        })

        // Community Detection
        val modularity: Modularity = new Modularity()
        modularity.execute(graphModel)

        val community = graphModel.getNodeTable().getColumn(Modularity.MODULARITY_CLASS)
        var community_map: HashMap[String, Int] = HashMap()
        gephi_graph.getNodes.toArray.foreach(node => community_map(node.getId.asInstanceOf[String]) = node.getAttribute(community).asInstanceOf[Int])
        
        // New Dataset Creation
        graph_in.data.map(hlist => addTo_dest(addTo_source(hlist, community_map(select_sourceID(hlist).toString)), community_map(select_destID(hlist).toString)))
    } 

    def only_keep_significant [
        S, SourceID, DestID, SComm, DComm, CType, IDType
    ](
        graph: Graph[S, SourceID, DestID], 
        community_att: Witness
    )(
        implicit
        source_id: SelectField.Aux[graph.Repr, Witness.`'source`.T :: SourceID :: HNil, SourceID, IDType],
        dest_id: SelectField.Aux[graph.Repr, Witness.`'dest`.T :: DestID :: HNil, DestID, IDType],
        source_community: SelectField.Aux[graph.Repr, Witness.`'source`.T :: community_att.T :: HNil, SComm, CType],
        dest_community: SelectField.Aux[graph.Repr, Witness.`'dest`.T :: community_att.T :: HNil, DComm, CType],
        res_model: IsValidGraph[graph.Repr, SourceID, DestID]
    ): Graph.Aux[graph.Repr, SourceID, DestID, res_model.Repr] = {
        var nb_edges = 0
        val community_sizes: HashMap[CType, HashSet[IDType]] = HashMap() 

        graph.data.foreach(edge => { nb_edges += 1 ; 
            val scomm = source_community(edge) ; val dcomm = dest_community(edge) 
            community_sizes(scomm) = community_sizes.getOrElse(scomm, HashSet()) + source_id(edge) 
            community_sizes(dcomm) = community_sizes.getOrElse(dcomm, HashSet()) + dest_id(edge) 
        })

        val resolution_limit = java.lang.Math.sqrt(2*nb_edges)
        res_model(graph.data.filter(hlist => 
            (community_sizes(source_community(hlist)).size > resolution_limit) && 
            (community_sizes(dest_community(hlist)).size > resolution_limit)
        ))
    }

    def only_keep_significant2 [
        S, SourceID, DestID, SComm, DComm, CType, IDType, NRepr <: HList
    ](
        graph: Graph[S, SourceID, DestID], 
        community_att: Witness
    )(
        implicit
        get_nodes: GetNodes.Aux[graph.Repr, Model.Relation, Relation.Aux[NRepr, NRepr]],
        get_community: SelectField.Aux[NRepr, community_att.T :: HNil, community_att.T, CType],
        source_community: SelectField.Aux[graph.Repr, Witness.`'source`.T :: community_att.T :: HNil, community_att.T, CType],
        dest_community: SelectField.Aux[graph.Repr, Witness.`'dest`.T :: community_att.T :: HNil, community_att.T, CType],
        res_model: IsValidGraph[graph.Repr, SourceID, DestID]
    ): Graph.Aux[graph.Repr, SourceID, DestID, res_model.Repr] = {
        val community_sizes: HashMap[CType, Int] = HashMap() 
        
        get_nodes(graph.data).data.foreach(node => { 
            val comm = get_community(node)
            community_sizes(comm) = community_sizes.getOrElse(comm, 0) + 1
        })        

        val resolution_limit = java.lang.Math.sqrt(2*graph.data.size)
        res_model(graph.data.filter(hlist => 
            (community_sizes(source_community(hlist)) > resolution_limit) && 
            (community_sizes(dest_community(hlist)) > resolution_limit)
        ))
    }




    // Fonction de test
    def get_community(node_id: Long): String = node_id match { case 1268486802949767200L => "C1" ; case 277430850L => "C1" ; case 1268486302459767200L => "C3" }
}