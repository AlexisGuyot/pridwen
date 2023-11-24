import shapeless.{HList, Witness, ::, HNil}

import pridwen.models._
import pridwen.models.aux.{SelectField, AddField}

import org.gephi.project.api.{ProjectController, Workspace}
import org.gephi.graph.api.{GraphController, GraphModel, DirectedGraph}
import org.gephi.statistics.plugin.Modularity
import org.openide.util.Lookup

import scala.collection.mutable.Map

object community {
    def detect_with_louvain [SourceSchema <: HList, DestSchema <: HList, EdgeSchema <: HList, Out0 <: HList, New_Schema <: HList](
        graph_in: Graph, 
        weight_att: Witness,
        comm_att: Witness
    )(
        implicit
        select_sourceID: SelectField[graph_in.Schema, Graph.SourceName :: graph_in.SourceID :: HNil],
        select_destID: SelectField[graph_in.Schema, Graph.DestName :: graph_in.DestID :: HNil],
        select_weight: SelectField.Aux[graph_in.Schema, Graph.EdgeName :: weight_att.T :: HNil, weight_att.T, Int],
        addTo_source: AddField.Aux[graph_in.Schema, Graph.SourceName :: HNil, comm_att.T, Int, Out0],
        addTo_dest: AddField.Aux[Out0, Graph.DestName :: HNil, comm_att.T, Int, New_Schema],
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

    def keep_significant [SComm, DComm, CType, IDType](
        graph: Graph, 
        community_att: Witness
    )(
        implicit
        same_schema: graph.SourceSchema =:= graph.DestSchema,
        get_community: SelectField.Aux[graph.SourceSchema, community_att.T :: HNil, community_att.T, CType],
        source_community: SelectField.Aux[graph.Schema, Graph.SourceName :: community_att.T :: HNil, community_att.T, CType],
        dest_community: SelectField.Aux[graph.Schema, Graph.DestName :: community_att.T :: HNil, community_att.T, CType],
        new_dataset: Graph.ValidSchema[graph.Schema, graph.SourceID, graph.DestID]
    ): new_dataset.T = {
        import scala.collection.parallel.CollectionConverters._

        val community_sizes: Map[CType, Int] = Map() 
        
        println("Tailles communautés")
        time { graph.nodes(same_schema).asList.foreach(node => { 
            val comm = get_community(node)
            community_sizes(comm) = community_sizes.getOrElse(comm, 0) + 1
        }) }        

        println("Création nouveau graphe significatif")
        val resolution_limit = java.lang.Math.sqrt(2*graph.data.size)
        time { new_dataset(time { graph.data.par.filter(hlist => 
            (community_sizes(source_community(hlist)) > resolution_limit) && 
            (community_sizes(dest_community(hlist)) > resolution_limit)
        ).toList }) }
    }




    // Fonctions de test
    def get_community(node_id: Long): String = node_id match { case 1268486802949767200L => "C1" ; case 277430850L => "C1" ; case 1268486302459767200L => "C3" }

    def community_from_file[Out0 <: HList, New_Schema <: HList, NodeID](graph: Graph)(
        implicit
        select_sourceID: SelectField.Aux[graph.Schema, Graph.SourceName :: graph.SourceID :: HNil, graph.SourceID, Double],
        select_destID: SelectField.Aux[graph.Schema, Graph.DestName :: graph.DestID :: HNil, graph.DestID, Double],
        addTo_source: AddField.Aux[graph.Schema, Graph.SourceName :: HNil, Witness.`'community`.T, Int, Out0],
        addTo_dest: AddField.Aux[Out0, Graph.DestName :: HNil, Witness.`'community`.T, Int, New_Schema]
    ): List[New_Schema] = {
        import scala.collection.parallel.CollectionConverters._

        println("Chargement XML")
        val g_rt = time { scala.xml.XML.loadFile("/home/alexis/Documents/Tweets/GMerged/g_rt") }
        val nodes: Map[Double, Int] = Map()
        println("Parcours XML")
        time { (g_rt \ "graph" \ "node").foreach(node => nodes((node \ "data").find(d => (d \@ "key") == "v_name").map(_.text).get.toDouble) = (node \ "data").find(d => (d \@ "key") == "v_community").map(_.text).get.toInt) }

        println("Création liste arêtes")
        time { graph.data.par.map(hlist => {
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