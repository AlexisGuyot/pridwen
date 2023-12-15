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

        println("--- Calculating the auxiliary matrices (substep 1/2)")
        val (nmc, mct, md, i, ni, inmc) = time {
            val nmc = time { comm_matrix.map(v => if(v == 0) 1 else 0) }
            val mct = time { comm_matrix.t }
            val md = time { adj_matrix * comm_matrix }
            val i = time { md.map(v => if(v == 0) 1 else 0) }
            val ni = time { md.map(v => if(v == 0) 0 else 1) } // Essayer de passer ni en DenseMatrix
            val inmc = time { i *:* nmc }
            (nmc, mct, md, i, ni, inmc) 
        }

        val (man, mp) = (CSCMatrix.zeros[Double](comm_matrix.cols, comm_matrix.cols), CSCMatrix.zeros[Double](comm_matrix.cols, comm_matrix.cols))

        println("--- Filling the antagonism and porosity matrices (substep 2/2)")
        time { (0 to (comm_matrix.cols-1) par).foreach( i => time {
            val ii = CSCMatrix.tabulate(comm_matrix.rows, comm_matrix.cols) { case (r, c) => inmc(r, c) * comm_matrix(r, i) }
            val tmp_mdsi = adj_matrix * ii
            val mdsi = CSCMatrix.tabulate(comm_matrix.rows, comm_matrix.cols){ case(r,c) => tmp_mdsi(r, c) * comm_matrix(r, i) * ni(r, c) }
            val mmdsi = mdsi.map(v => if(v != 0) 1.0 else 0)
            val mvani = CSCMatrix.tabulate(comm_matrix.rows, comm_matrix.cols){ case(r,c) => if(mdsi(r, c) != 0) if(mdsi(r, c) + md(r, c) != 0) (mdsi(r, c).asInstanceOf[Double] / (mdsi(r, c) + md(r, c))) - 0.5 else 0 else 0 }
            val tmp_mpi1 = mvani.map(v => if(v < 0) 1.0 else 0)
            val mbsi = sum(mmdsi.toDenseMatrix, Axis._0)
            val mcti = CSCMatrix.tabulate(1, comm_matrix.rows){ case(r,c) => mct(i,c).asInstanceOf[Double] }

            val (mani1, mani2, mpi1) = (mcti * mvani, mcti * mmdsi, mcti * tmp_mpi1)

            for(j <- 0 to (comm_matrix.cols-1)) {
                man(i,j) = if(mani2(0, j) != 0) mani1(0, j) / mani2(0, j) else 0
                mp(i,j) = mpi1(0, j) / (if(mbsi(j) != 0) mbsi(j) else 1) * 100
            }
        }) }

        (man.toDenseMatrix, mp.toDenseMatrix)
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
        // Fills auxiliary maps by scanning the graph edges
        println("--- Constructing the auxiliary data structures (substep 1/2)")

        val comm_map: Map[NodeIDType, CommunityType] = Map()            // A map associating each vertex with its community
        val rev_adj_map: Map[NodeIDType, Map[NodeIDType, Int]] = Map()  // A map associating each vertex with its incoming edges (adjacency matrix transpose as a map)
        
        val (nodes, communities, nodeID_to_ind, rev_adj_alt) = time {
            time { graph.data.foreach(hlist => {
                val sid = select_sourceID(hlist) ; val did = select_destID(hlist)
                val scomm = select_source_community(hlist); val dcomm = select_dest_community(hlist)
                
                comm_map(sid) = scomm ; comm_map(did) = dcomm

                rev_adj_map(did) = rev_adj_map.getOrElse(did, Map())
                rev_adj_map(did)(sid) = rev_adj_map(did).getOrElse(sid, 0) + select_weight(hlist)
                rev_adj_map(sid) = rev_adj_map.getOrElse(sid, Map())
            }) }
            // nodes: indexed sequence of the graph nodes
            val nodes = time { rev_adj_map.keys.toIndexedSeq }
            // communities: indexed sequence of the graph communities, ordered by size (desc)
            val communities = time { comm_map.values.groupBy(x => x).toIndexedSeq.sortWith(_._2.size > _._2.size).map(_._1) }
            // nodeID_to_ind: a map associating each node with its index in the adjacency (row/col) and community (row) matrices
            val nodeID_to_ind = time { nodes.foldLeft((0, Map[NodeIDType, Int]())){ case ((i, res), node) => (i+1, res += (node -> i)) }._2 }
            // rev_adj_alt: an alternate version of rev_adj_map ordered consistently with the future matrices
            val rev_adj_alt = time { rev_adj_map.par.mapValues(m => m.toIndexedSeq.map{ case (k, v) => (nodeID_to_ind(k), v) }.sortWith((x, y) => x._1 < y._1).foldLeft(Queue[Int](), Queue[Int]()){ case ((keys, values), (k, v)) => (keys :+ k, values :+ v) }) }
            (nodes, communities, nodeID_to_ind, rev_adj_alt)
        }

        println(s"Significant communities: ${communities.map(_.asInstanceOf[Int]).mkString(", ")}")
        
        // Computes the adjacency and community matrices by building the auxiliary arrays forming the CSC Matrices
        println("--- Constructing the adjacency and community matrices (substep 2/2)")
        val nb_nodes = nodes.length ; val nb_comm = communities.length

        time {
            // Auxiliary arrays for the adjacency matrix
            val (data, indices, indptr, _) = time { nodes.foldLeft((Queue[Int](), Queue[Int](), Queue[Int](), 0)){ case ((d, ind, iptr, i), n) => {
                val rev_adj_n = rev_adj_alt(n)
                (d :++ rev_adj_n._2, ind :++ rev_adj_n._1, iptr :+ i, i + rev_adj_n._1.size )
            }} }

            // Auxiliary arrays for the community matrix
            val (indicesc, indptrc, _) = time { comm_map.groupMap(_._2)(_._1).toSeq.sortWith(_._2.size > _._2.size).foldLeft((Queue[Int](), Queue[Int](), 0)){case ((ind, iptr, i), (community, members)) => 
                (ind :++ members.map(m => nodeID_to_ind(m)), iptr :+ i, i + members.size )
            } }

            (
                new CSCMatrix(data.toArray, nb_nodes, nb_nodes, (indptr :+ data.size).toArray, indices.toArray),                // Adjacency matrix
                new CSCMatrix(Array.fill(nb_nodes){1}, nb_nodes, nb_comm, (indptrc :+ nb_nodes).toArray, indicesc.toArray)      // Community matrix
            )
        }
    }
}