import pridwen.models.Graph
import pridwen.models.aux.SelectField

import shapeless.{Witness, ::, HNil}

import scala.collection.mutable.Map
import scala.collection.immutable.Queue
import scala.collection.parallel.CollectionConverters._

import scala.reflect.ClassTag

import breeze.linalg.{CSCMatrix, sum, Axis}

import time._

object polarisation {
    def compute(ma: CSCMatrix[Int], mc: CSCMatrix[Int]) = {
        import scala.language.postfixOps

        println("Création de nmc et mct") ; val (nmc, mct) = time { (mc.map(v => if(v == 0) 1 else 0), mc.t) }
        println("Calcul md") ; val md = time { ma * mc }
        println("Création de i et ni") ; val (i, ni) = time { (md.map(v => if(v == 0) 1 else 0), md.map(v => if(v == 0) 0 else 1)) } // Essayer de passer ni en DenseMatrix
        println("Calcul inmc") ; val inmc = time { i *:* nmc }
        println("Man et Mp vides") ;val (man, mp) = (time { CSCMatrix.zeros[Double](mc.cols, mc.cols) }, time { CSCMatrix.zeros[Double](mc.cols, mc.cols) })

        println("Calcul lignes matrices ant et por")
        time { (0 to (mc.cols-1) par).foreach( i => {
        //time { (0 to (mc.cols-1)).foreach( i => {
            println(s"Communauté ${i} : calcul de ii") ; val ii = time { CSCMatrix.tabulate(mc.rows, mc.cols) { case (r, c) => inmc(r, c) * mc(r, i) } }
            println(s"Communauté ${i} : calcul de tmp_mdsi") ; val tmp_mdsi = time { ma * ii }
            println(s"Communauté ${i} : matrices intermédiaires")
            val mdsi = time { CSCMatrix.tabulate(mc.rows, mc.cols){ case(r,c) => tmp_mdsi(r, c) * mc(r, i) * ni(r, c) } }
            val mmdsi = time { mdsi.map(v => if(v != 0) 1.0 else 0) }
            val mvani = time { CSCMatrix.tabulate(mc.rows, mc.cols){ case(r,c) => if(mdsi(r, c) != 0) if(mdsi(r, c) + md(r, c) != 0) (mdsi(r, c).asInstanceOf[Double] / (mdsi(r, c) + md(r, c))) - 0.5 else 0 else 0 } }
            val tmp_mpi1 = time { mvani.map(v => if(v < 0) 1.0 else 0) }
            val mbsi = time { sum(mvani.map(v => if(v != 0) 1.0 else 0).toDenseMatrix, Axis._0) }
            val mcti = time { CSCMatrix.tabulate(1, mc.rows){ case(r,c) => mct(i,c).asInstanceOf[Double] } }

            println(s"Communauté ${i} : calcul de mani1") ; val mani1 = time { mcti * mvani }
            println(s"Communauté ${i} : calcul de mani2") ; val mani2 = time { mcti * mmdsi }
            println(s"Communauté ${i} : calcul de mpi1") ; val mpi1 = time { mcti * tmp_mpi1 }

            println(s"Communauté ${i} : remplissage de man et mp")
            time { for(j <- 0 to (mc.cols-1)) {
                man(i,j) = if(mani2(0, j) != 0) mani1(0, j)/mani2(0, j) else 0
                mp(i,j) = mpi1(0, j) / (if(mbsi(j) != 0) mbsi(j) else 1) * 100
            } }
        }) }

        println("Conversion en matrices pleines") ; (time { man.toDenseMatrix }, time { mp.toDenseMatrix })
    }


    def get_matrices[CommT: ClassTag, NodeT: ClassTag](graph: Graph, weight_att: Witness, comm_att: Witness)(
        implicit
        source_id: SelectField.Aux[graph.Schema, Graph.SourceName :: graph.SourceID :: HNil, graph.SourceID, NodeT],
        dest_id: SelectField.Aux[graph.Schema, Graph.DestName :: graph.DestID :: HNil, graph.DestID, NodeT],
        source_community: SelectField.Aux[graph.Schema, Graph.SourceName :: comm_att.T :: HNil, comm_att.T, CommT],
        dest_community: SelectField.Aux[graph.Schema, Graph.DestName :: comm_att.T :: HNil, comm_att.T, CommT],
        get_weight: SelectField.Aux[graph.Schema, Graph.EdgeName :: weight_att.T :: HNil, weight_att.T, Int],
    ): (CSCMatrix[Int], CSCMatrix[Int]) = {
        val comm_map: Map[NodeT, CommT] = Map()
        val rev_adj_map: Map[NodeT, Map[NodeT, Int]] = Map()

        println("Construction hashmaps")
        time { graph.data.foreach(hlist => {
            val sid = source_id(hlist) ; val did = dest_id(hlist)
            val scomm = source_community(hlist); val dcomm = dest_community(hlist)
            
            comm_map(sid) = scomm ; comm_map(did) = dcomm

            rev_adj_map(did) = rev_adj_map.getOrElse(did, Map())
            rev_adj_map(did)(sid) = rev_adj_map(did).getOrElse(sid, 0) + get_weight(hlist)
            rev_adj_map(sid) = rev_adj_map.getOrElse(sid, Map())
        }) }

        val nodes = rev_adj_map.keys.toIndexedSeq ; val nb_nodes = nodes.length 
        val (_, nodeID_to_ind) = time { nodes.foldLeft((0, Map[NodeT, Int]())){ case ((i, res), node) => (i+1, res += (node -> i)) } }
        val communities = comm_map.values.toIndexedSeq.distinct ; val nb_comm = communities.length
        
        println("Construction matrices")
        val (data, indices, indptr, _) = time { rev_adj_map.foldLeft((Queue[Int](), Queue[Int](), Queue[Int](), 0)){ case ((d, ind, iptr, i), (dest, in)) => {
            val keys = in.keys
            (d :++ in.values, ind :++ keys.map(k => nodeID_to_ind(k)), iptr :+ i, i + keys.size )
        }} }
        val adj_matrix = time { new CSCMatrix(data.toArray, nb_nodes, nb_nodes, (indptr :+ data.size).toArray, indices.toArray) }

        val (indicesc, indptrc, _) = time { comm_map.groupMap(_._2)(_._1).toSeq.sortWith(_._2.size > _._2.size).foldLeft((Queue[Int](), Queue[Int](), 0)){case ((ind, iptr, i), (community, members)) => 
            (ind :++ members.map(m => nodeID_to_ind(m)), iptr :+ i, i + members.size )
        } }
        val comm_matrix = time { new CSCMatrix(Array.fill(nb_nodes){1}, nb_nodes, nb_comm, (indptrc :+ nb_nodes).toArray, indicesc.toArray) }

        (adj_matrix, comm_matrix)
    }
}