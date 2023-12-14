import shapeless.{HList, Witness, ::, HNil}

import pridwen.models._
import pridwen.schemaop.{SelectField, AddField}

import org.gephi.project.api.{ProjectController, Workspace}
import org.gephi.graph.api.{GraphController, GraphModel, DirectedGraph}
import org.gephi.statistics.plugin.Modularity
import org.openide.util.Lookup

import scala.collection.mutable.Map
import scala.collection.parallel.CollectionConverters._

object community {
    /**
    * Detects communities in a graph with the Louvain method (https://en.wikipedia.org/wiki/Louvain_method).
    * @tparam NewSchema1 [Inferred] The new schema obtained after adding the community attribute to the source vertices of the graph.
    * @tparam NewSchema2 [Inferred] The new schema obtained after adding the community attribute to the destination vertices of the graph.
    * @param graph The graph in which communities should be detected.
    * @param weight_att The name of the weight attribute in this graph.
    * @param comm_att The name that should be given to the new community attribute in the graph vertices.
    * @param select_sourceID [Implicit] If exists, provides a function (apply) for selecting the source vertex ID in the graph data.
    * @param select_destID [Implicit] If exists, provides a function (apply) for selecting the destination vertex ID in the graph data.
    * @param select_weight [Implicit] If exists, provides a function (apply) for selecting the edge weight in the graph data.
    * @param addTo_source [Implicit] If exists, provides a function (apply) for adding a new community attribute in the source vertex attributes.
    * @param addTo_dest [Implicit] If exists, provides a function (apply) for adding a new community attribute in the destination vertex attributes.
    * @return a new unmodeled dataset with the same schema as the input graph + the new community attributes.
    */
    def detect_with_louvain [NewSchema1 <: HList, NewSchema2 <: HList](
        graph: Graph, 
        weight_att: Witness,
        comm_att: Witness
    )(
        implicit
        select_sourceID: SelectField[graph.Schema, Graph.SourceName :: graph.SourceID :: HNil],
        select_destID: SelectField[graph.Schema, Graph.DestName :: graph.DestID :: HNil],
        select_weight: SelectField.Aux[graph.Schema, Graph.EdgeName :: weight_att.T :: HNil, weight_att.T, Int],
        addTo_source: AddField.Aux[graph.Schema, Graph.SourceName :: HNil, comm_att.T, Int, NewSchema1],
        addTo_dest: AddField.Aux[NewSchema1, Graph.DestName :: HNil, comm_att.T, Int, NewSchema2],
    ): List[NewSchema2] = {
        // Init Gephi Toolkit
        val pc: ProjectController = Lookup.getDefault().lookup(classOf[ProjectController])
        pc.newProject()
        val workspace: Workspace = pc.getCurrentWorkspace()
        val graphModel: GraphModel = Lookup.getDefault().lookup(classOf[GraphController]).getGraphModel()

        // Gephi Graph Construction
        val gephi_graph: DirectedGraph = graphModel.getDirectedGraph()
        graph.data.foreach(hlist => {
            val sourceID = select_sourceID(hlist).toString ; val destID = select_destID(hlist).toString
            val source = Option(gephi_graph.getNode(sourceID)) getOrElse { val tmp = graphModel.factory().newNode(sourceID) ; gephi_graph.addNode(tmp) ; tmp }
            val dest = Option(gephi_graph.getNode(destID)) getOrElse { val tmp = graphModel.factory().newNode(destID) ; gephi_graph.addNode(tmp) ; tmp }
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
        graph.data.map(hlist => addTo_dest(addTo_source(hlist, community_map(select_sourceID(hlist).toString)), community_map(select_destID(hlist).toString)))
    } 

    /**
    * Removes all the vertices that are not included in the significant communities of a graph.
    * @tparam CommunityType [Inferred] The type of the community attribute in the vertices schema (should be the same in source and destination vertices).
    * @param graph The graph in which vertices should be removed if they are not part of a significant community.
    * @param community_att The name of the community attribute in this graph.
    * @param same_schema [Implicit] If exists, all the vertices of this graph share the same schema. Provides a way to cast data of type SourceSchema into data of type DestSchema (and vice versa).
    * @param select_community [Implicit] If exists, provides a function (apply) for selecting the community attribute in the list of nodes of the graph.
    * @param select_source_community [Implicit] If exists, provides a function (apply) for selecting the community attribute in the graph data.
    * @param select_dest_community [Implicit] If exists, provides a function (apply) for selecting the community attribute in the graph data.
    * @param new_dataset [Implicit] If exists, provides a function (apply) for modelling data as a graph with the same schema, source ID and destination ID as the input one.
    * @return a new graph modelled with the new_dataset parameter
    */
    def keep_significant[CommunityType](
        graph: Graph, 
        community_att: Witness
    )(
        implicit
        same_schema: graph.SourceSchema =:= graph.DestSchema,
        select_community: SelectField.Aux[graph.SourceSchema, community_att.T :: HNil, community_att.T, CommunityType],
        select_source_community: SelectField.Aux[graph.Schema, Graph.SourceName :: community_att.T :: HNil, community_att.T, CommunityType],
        select_dest_community: SelectField.Aux[graph.Schema, Graph.DestName :: community_att.T :: HNil, community_att.T, CommunityType],
        new_dataset: Graph.ValidSchema[graph.Schema, graph.SourceID, graph.DestID]
    ): new_dataset.T = {
        val community_sizes: Map[CommunityType, Int] = Map() 
        
        // Fills in a map associating each community with its size
        println("Tailles communautés")
        time { graph.nodes(same_schema).asList.foreach(node => { 
            val comm = select_community(node)
            community_sizes(comm) = community_sizes.getOrElse(comm, 0) + 1
        }) }        

        // Creates a new graph by filtering the nodes of the input graph that are not members of a significant community (community size < resolution limit of the graph)
        println("Création nouveau graphe significatif")
        val resolution_limit = java.lang.Math.sqrt(2*graph.data.size)
        time { new_dataset(time { graph.data.par.filter(hlist => 
            (community_sizes(select_source_community(hlist)) > resolution_limit) && 
            (community_sizes(select_dest_community(hlist)) > resolution_limit)
        ).toList }) }
    }




    // Temporary test functions

    def community_from_file[Out0 <: HList, New_Schema <: HList, NodeID](graph: Graph)(
        implicit
        select_sourceID: SelectField.Aux[graph.Schema, Graph.SourceName :: graph.SourceID :: HNil, graph.SourceID, Double],
        select_destID: SelectField.Aux[graph.Schema, Graph.DestName :: graph.DestID :: HNil, graph.DestID, Double],
        addTo_source: AddField.Aux[graph.Schema, Graph.SourceName :: HNil, Witness.`'community`.T, Int, Out0],
        addTo_dest: AddField.Aux[Out0, Graph.DestName :: HNil, Witness.`'community`.T, Int, New_Schema]
    ): List[New_Schema] = {
        println("Chargement XML")
        val g_rt = time { scala.xml.XML.loadFile("/home/alexis/Documents/Tweets/GMerged/g_rt") }
        //val g_rt = time { scala.xml.XML.loadFile("/home/alexis/Documents/Tweets/GMerged/g_rt4") }
        val nodes: Map[Double, Int] = Map()
        println("Parcours XML")
        time { (g_rt \ "graph" \ "node").foreach(node => nodes((node \ "data").find(d => (d \@ "key") == "v_name").map(_.text).get.toDouble) = (node \ "data").find(d => (d \@ "key") == "v_community").map(_.text).get.toInt) }

        println("Création liste arêtes")
        time { graph.data.par.map(hlist => {
        //time { graph.data.par.filter(hlist => nodes.contains(select_sourceID(hlist)) && nodes.contains(select_destID(hlist))).map(hlist => {
            val sourceID = select_sourceID(hlist) ; val destID = select_destID(hlist)
            addTo_dest(addTo_source(hlist, nodes(sourceID)), nodes(destID))
        }).toList }
    }

    // Source : https://biercoff.com/easily-measuring-code-execution-time-in-scala/
    private def time[R](block: => R): R = {
        val t0 = System.nanoTime()
        val result = block    // call-by-name
        val t1 = System.nanoTime()
        println("Elapsed time: " + (t1 - t0) + " ns")
        result
    } 
}