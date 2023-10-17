import shapeless.{HList, Witness, ::, HNil}
import shapeless.labelled.{field}

import pridwen.models.{Graph}
import pridwen.models.aux.{SelectField, AddField}
import pridwen.support.functions.{get}

import org.gephi.project.api.{ProjectController, Workspace}
import org.gephi.graph.api.{GraphController, GraphModel, DirectedGraph}
import org.gephi.statistics.plugin.{Modularity}
import org.openide.util.Lookup

import scala.collection.mutable.Map

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
        var community_map: Map[String, Int] = Map()
        gephi_graph.getNodes.toArray.foreach(node => community_map(node.getId.asInstanceOf[String]) = node.getAttribute(community).asInstanceOf[Int])
        
        // New Dataset Creation
        graph_in.data.map(hlist => addTo_dest(addTo_source(hlist, community_map(select_sourceID(hlist).toString)), community_map(select_destID(hlist).toString)))
    } 



    // Fonction de test
    def get_community(node_id: Long): String = node_id match { case 1268486802949767200L => "C1" ; case 277430850L => "C1" ; case 1268486302459767200L => "C3" }
}