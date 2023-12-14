import pridwen.models.Graph
import pridwen.schemaop.SelectField

import shapeless.{Witness, ::, HNil}

import scala.collection.mutable.Map
import scala.collection.immutable.Queue
import scala.collection.parallel.CollectionConverters._

import breeze.linalg.{CSCMatrix, sum, Axis}

import time._

object polarisation {
    /**
    * Computes the antagonism and porosity matrices associated with a social graph with the ERIS method (https://hal.science/hal-03889719/).
    * @param adj_matrix The adjacency matrix of the graph (sparse).
    * @param comm_matrix The community matrix, a matrix of 0/1 in which each cell (i,j) indicates if the node i belongs to the community j (1) or not (0) (sparse).
    * Please refer to the paper for more information on the intermediate matrices (sizes, types, meaning).
    * @return the antagonism and porosity matrices, as a pair of sparse matrices.
    */
    def compute(adj_matrix: CSCMatrix[Int], comm_matrix: CSCMatrix[Int]) = {
        import scala.language.postfixOps

        //data.write_matrix(adj_matrix, "adj")
        //data.write_matrix(comm_matrix, "comm")

        println("Création de nmc et mct") ; val (nmc, mct) = time { (comm_matrix.map(v => if(v == 0) 1 else 0), comm_matrix.t) }
        println("Calcul md") ; val md = time { adj_matrix * comm_matrix }
        println("Création de i et ni") ; val (i, ni) = time { (md.map(v => if(v == 0) 1 else 0), md.map(v => if(v == 0) 0 else 1)) } // Essayer de passer ni en DenseMatrix
        println("Calcul inmc") ; val inmc = time { i *:* nmc }
        println("Man et Mp vides") ;val (man, mp) = (time { CSCMatrix.zeros[Double](comm_matrix.cols, comm_matrix.cols) }, time { CSCMatrix.zeros[Double](comm_matrix.cols, comm_matrix.cols) })

        //data.write_matrix(md, "md")
        //data.write_matrix(i, "i")

        println("Calcul lignes matrices ant et por")
        time { (0 to (comm_matrix.cols-1) par).foreach( i => {
        //time { (0 to (comm_matrix.cols-1)).foreach( i => {
            println(s"Communauté ${i} : calcul de ii") ; val ii = time { CSCMatrix.tabulate(comm_matrix.rows, comm_matrix.cols) { case (r, c) => inmc(r, c) * comm_matrix(r, i) } }
            println(s"Communauté ${i} : calcul de tmp_mdsi") ; val tmp_mdsi = time { adj_matrix * ii }
            println(s"Communauté ${i} : matrices intermédiaires")
            val mdsi = time { CSCMatrix.tabulate(comm_matrix.rows, comm_matrix.cols){ case(r,c) => tmp_mdsi(r, c) * comm_matrix(r, i) * ni(r, c) } }
            val mmdsi = time { mdsi.map(v => if(v != 0) 1.0 else 0) }
            val mvani = time { CSCMatrix.tabulate(comm_matrix.rows, comm_matrix.cols){ case(r,c) => if(mdsi(r, c) != 0) if(mdsi(r, c) + md(r, c) != 0) (mdsi(r, c).asInstanceOf[Double] / (mdsi(r, c) + md(r, c))) - 0.5 else 0 else 0 } }
            val tmp_mpi1 = time { mvani.map(v => if(v < 0) 1.0 else 0) }
            val mbsi = time { sum(mmdsi.toDenseMatrix, Axis._0) } 
            val mcti = time { CSCMatrix.tabulate(1, comm_matrix.rows){ case(r,c) => mct(i,c).asInstanceOf[Double] } }

            println(s"Communauté ${i} : calcul de mani1") ; val mani1 = time { mcti * mvani }// ; println(mani1.toDenseMatrix)
            println(s"Communauté ${i} : calcul de mani2") ; val mani2 = time { mcti * mmdsi }// ; println(mani2.toDenseMatrix)
            println(s"Communauté ${i} : calcul de mpi1") ; val mpi1 = time { mcti * tmp_mpi1 }// ; println(mpi1.toDenseMatrix)

            println(s"Communauté ${i} : remplissage de man et mp")
            time { for(j <- 0 to (comm_matrix.cols-1)) {
                man(i,j) = if(mani2(0, j) != 0) mani1(0, j)/mani2(0, j) else 0
                mp(i,j) = mpi1(0, j) / (if(mbsi(j) != 0) mbsi(j) else 1) * 100
            } }
        }) }

        println("Conversion en matrices pleines") ; (time { man.toDenseMatrix }, time { mp.toDenseMatrix })
    }

    /**
    * Computes the adjacency and community matrices required by the ERIS method (https://hal.science/hal-03889719/).
    * @tparam CommunityType [Inferred] The type of the community attribute in the vertices schema (should be the same in source and destination vertices).
    * @tparam NodeIDType [Inferred] The type of the ID attribute in the vertices schema (should be the same in source and destination vertices).
    * @param graph The graph in which polarisation should be assessed.
    * @param weight_att The name of the weight attribute in this graph.
    * @param comm_att The name of the community attribute in the graph.
    * @param select_sourceID [Implicit] If exists, provides a function (apply) for selecting the source vertex ID in the graph data.
    * @param select_destID [Implicit] If exists, provides a function (apply) for selecting the destination vertex ID in the graph data.
    * @param select_source_community [Implicit] If exists, provides a function (apply) for selecting the community attribute in the graph data.
    * @param select_dest_community [Implicit] If exists, provides a function (apply) for selecting the community attribute in the graph data.
    * @param select_weight [Implicit] If exists, provides a function (apply) for selecting the edge weight in the graph data.
    * @return the adjacency and community matrices, as a pair of sparse matrices.
    */
    def get_matrices[CommunityType, NodeIDType](
        graph: Graph, 
        weight_att: Witness, 
        comm_att: Witness
    )(
        implicit
        select_sourceID: SelectField.Aux[graph.Schema, Graph.SourceName :: graph.SourceID :: HNil, graph.SourceID, NodeIDType],
        select_destID: SelectField.Aux[graph.Schema, Graph.DestName :: graph.DestID :: HNil, graph.DestID, NodeIDType],
        select_source_community: SelectField.Aux[graph.Schema, Graph.SourceName :: comm_att.T :: HNil, comm_att.T, CommunityType],
        select_dest_community: SelectField.Aux[graph.Schema, Graph.DestName :: comm_att.T :: HNil, comm_att.T, CommunityType],
        select_weight: SelectField.Aux[graph.Schema, Graph.EdgeName :: weight_att.T :: HNil, weight_att.T, Int]
    ): (CSCMatrix[Int], CSCMatrix[Int]) = {
        val comm_map: Map[NodeIDType, CommunityType] = Map()            // A map associating each vertex with its community
        val adj_map: Map[NodeIDType, Map[NodeIDType, Int]] = Map()      // A map associating each vertex with its outcoming edges

        // Fills in the above maps by scanning the graph edges
        println("Construction hashmaps")
        time { graph.data.foreach(hlist => {
            val sid = select_sourceID(hlist) ; val did = select_destID(hlist)
            val scomm = select_source_community(hlist); val dcomm = select_dest_community(hlist)
            
            comm_map(sid) = scomm ; comm_map(did) = dcomm

            adj_map(sid) = adj_map.getOrElse(sid, Map())
            adj_map(sid)(did) = adj_map(sid).getOrElse(did, 0) + select_weight(hlist)
            adj_map(did) = adj_map.getOrElse(did, Map())
        }) }

        // Gets the sets of nodes and communities of the graph
        //val nodes = adj_map.keys.toIndexedSeq.sortWith((x, y) => f"${x.asInstanceOf[Double]}%.0f" < f"${y.asInstanceOf[Double]}%.0f") ; val nb_nodes = nodes.length 
        //val nodes = adj_map.keys.toIndexedSeq.sortWith((x, y) => x.asInstanceOf[Double] < y.asInstanceOf[Double]) ; val nb_nodes = nodes.length 
        val nodes = adj_map.keys.toIndexedSeq ; val nb_nodes = nodes.length 
        val communities = comm_map.values.groupBy(x => x).toIndexedSeq.sortWith(_._2.size > _._2.size).map(_._1) ; val nb_comm = communities.length

        println(s"Significative communities: ${communities.map(_.asInstanceOf[Int]).mkString(", ")}")

        // Fills a map associating each vertex with its index in the nodes list
        //val (_, nodeID_to_ind) = time { nodes.sortWith(_.asInstanceOf[Double] < _.asInstanceOf[Double]).foldLeft((0, Map[NodeIDType, Int]())){ case ((i, res), node) => (i+1, res += (node -> i)) } }
        val nodeID_to_ind = nodes.foldLeft((0, Map[NodeIDType, Int]())){ case ((i, res), node) => (i+1, res += (node -> i)) }._2
        val commID_to_ind = communities.foldLeft((0, Map[CommunityType, Int]())){ case ((i, res), community) => (i+1, res += (community -> i)) }._2
        
        // Computes the adjacency matrix by building the auxiliary arrays forming the CSC Matrix
        println("Construction matrices")
        val adj_matrix = CSCMatrix.zeros[Int](nb_nodes, nb_nodes)
        nodes.foreach(n => adj_map(n).foreach{ case (v,w) => adj_matrix(nodeID_to_ind(n), nodeID_to_ind(v)) = w })

        // Computes the community matrix by building the auxiliary arrays forming the CSC Matrix
        val comm_matrix = CSCMatrix.zeros[Int](nb_nodes, nb_comm)
        nodes.foreach(n => comm_matrix(nodeID_to_ind(n), commID_to_ind(comm_map(n))) = 1)

        (adj_matrix, comm_matrix)
    }

    /* def get_matrices[CommunityType, NodeIDType](
        graph: Graph, 
        weight_att: Witness, 
        comm_att: Witness
    )(
        implicit
        select_sourceID: SelectField.Aux[graph.Schema, Graph.SourceName :: graph.SourceID :: HNil, graph.SourceID, NodeIDType],
        select_destID: SelectField.Aux[graph.Schema, Graph.DestName :: graph.DestID :: HNil, graph.DestID, NodeIDType],
        select_source_community: SelectField.Aux[graph.Schema, Graph.SourceName :: comm_att.T :: HNil, comm_att.T, CommunityType],
        select_dest_community: SelectField.Aux[graph.Schema, Graph.DestName :: comm_att.T :: HNil, comm_att.T, CommunityType],
        select_weight: SelectField.Aux[graph.Schema, Graph.EdgeName :: weight_att.T :: HNil, weight_att.T, Int]
    ): (CSCMatrix[Int], CSCMatrix[Int]) = {
        val comm_map: Map[NodeIDType, CommunityType] = Map()            // A map associating each vertex with its community
        val rev_adj_map: Map[NodeIDType, Map[NodeIDType, Int]] = Map()  // A map associating each vertex with its incoming edges (adjacency matrix transpose as a map)

        // Fills in the above maps by scanning the graph edges
        println("Construction hashmaps")
        time { graph.data.foreach(hlist => {
            val sid = select_sourceID(hlist) ; val did = select_destID(hlist)
            val scomm = select_source_community(hlist); val dcomm = select_dest_community(hlist)
            
            comm_map(sid) = scomm ; comm_map(did) = dcomm

            rev_adj_map(did) = rev_adj_map.getOrElse(did, Map())
            rev_adj_map(did)(sid) = rev_adj_map(did).getOrElse(sid, 0) + select_weight(hlist)
            rev_adj_map(sid) = rev_adj_map.getOrElse(sid, Map())
        }) }

        // Gets the sets of nodes and communities of the graph
        val nodes = rev_adj_map.keys.toIndexedSeq ; val nb_nodes = nodes.length 
        val communities = comm_map.values.toIndexedSeq.distinct ; val nb_comm = communities.length

        // Fills a map associating each vertex with its index in the nodes list
        val (_, nodeID_to_ind) = time { nodes.foldLeft((0, Map[NodeIDType, Int]())){ case ((i, res), node) => (i+1, res += (node -> i)) } }
        
        // Computes the adjacency matrix by building the auxiliary arrays forming the CSC Matrix
        println("Construction matrices")
        val (data, indices, indptr, _) = time { rev_adj_map.foldLeft((Queue[Int](), Queue[Int](), Queue[Int](), 0)){ case ((d, ind, iptr, i), (dest, in)) => {
            val keys = in.keys
            (d :++ in.values, ind :++ keys.map(k => nodeID_to_ind(k)), iptr :+ i, i + keys.size )
        }} }
        val adj_matrix = time { new CSCMatrix(data.toArray, nb_nodes, nb_nodes, (indptr :+ data.size).toArray, indices.toArray) }

        // Computes the community matrix by building the auxiliary arrays forming the CSC Matrix
        val (indicesc, indptrc, _) = time { comm_map.groupMap(_._2)(_._1).toSeq.sortWith(_._2.size > _._2.size).foldLeft((Queue[Int](), Queue[Int](), 0)){case ((ind, iptr, i), (community, members)) => 
            (ind :++ members.map(m => nodeID_to_ind(m)), iptr :+ i, i + members.size )
        } }
        val comm_matrix = time { new CSCMatrix(Array.fill(nb_nodes){1}, nb_nodes, nb_comm, (indptrc :+ nb_nodes).toArray, indicesc.toArray) }

        (adj_matrix, comm_matrix)
    } */
}