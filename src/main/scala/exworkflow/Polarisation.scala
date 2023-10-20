import pridwen.models.{Graph, GetNodes}
import pridwen.models.aux.{SelectField}

import shapeless.{Witness, ::, HNil}

import scala.collection.mutable.{HashMap, HashSet}
import scala.collection.parallel.immutable.ParVector
import breeze.linalg._

import scala.reflect.{ClassTag}

object polarisation {
    def compute[NodeID: ClassTag, CommID: ClassTag](
        adj: HashMap[NodeID, HashMap[NodeID, Int]], 
        comm: HashMap[NodeID, HashMap[CommID, Boolean]]
    ) = {
        val ma = to_adjacency_matrix(adj)
        val (mc, nmc, mct) = to_community_matrix(comm)

        val md = ma * mc 

        val i = CSCMatrix.tabulate(md.rows, md.cols){ case (r, c) => if(md(r,c) == 0) 1 else 0 }
        val ni = CSCMatrix.tabulate(md.rows, md.cols){ case (r, c) => if(md(r,c) == 0) 0 else 1 }

        val inmc = i *:* nmc

        val man = CSCMatrix.zeros[Double](mc.cols, mc.cols)
        val mp = CSCMatrix.zeros[Double](mc.cols, mc.cols)

        ParVector.range(0, mc.cols).foreach( i => {
        //for(i <- 0 to (mc.cols-1)) {
            val ii = CSCMatrix.tabulate(mc.rows, mc.cols) { case (r, c) => inmc(r, c) * mc(r, i) }
            val tmp_mdsi = ma * ii
            val mbsi = DenseVector.zeros[Double](mc.cols)

            val mdsi = CSCMatrix.zeros[Double](mc.rows, mc.cols)
            val mmdsi = CSCMatrix.zeros[Double](mc.rows, mc.cols)
            val mvani = CSCMatrix.zeros[Double](mc.rows, mc.cols)
            val tmp_mpi1 = CSCMatrix.zeros[Double](mc.rows, mc.cols)
            val mcti = CSCMatrix.zeros[Double](1, mc.rows)

            for(r <- 0 to (mc.rows-1)) for(c <- 0 to (mc.cols-1)) {
                mdsi(r, c) = tmp_mdsi(r, c) * mc(r, i) * ni(r, c)
                mmdsi(r, c) = if(mdsi(r,c) != 0) 1.0 else 0
                mvani(r, c) = if(mdsi(r, c) != 0) if(mdsi(r, c) + md(r, c) != 0) (mdsi(r, c).asInstanceOf[Double] / (mdsi(r, c) + md(r, c))) - 0.5 else 0 else 0
                tmp_mpi1(r, c) = if(mvani(r, c) < 0) 1.0 else 0
                mbsi(c) += (if(mvani(r, c) != 0) 1 else 0)
                mcti(0, r) = mct(i, r).asInstanceOf[Double]
            }

            val mani1 = mcti * mvani
            val mani2 = mcti * mmdsi
            val mpi1 = mcti * tmp_mpi1

            for(j <- 0 to (mc.cols-1)) {
                man(i,j) = if(mani2(0, j) != 0) mani1(0, j)/mani2(0, j) else 0
                mp(i,j) = mpi1(0, j) / (if(mbsi(j) != 0) mbsi(j) else 1) * 100
            }
        })

        (man.toDenseMatrix, mp.toDenseMatrix)
    }

    def compute(ma: CSCMatrix[Int], mc: CSCMatrix[Int]) = {
        val nmc = CSCMatrix.zeros[Int](mc.rows, mc.cols)
        val mct = CSCMatrix.zeros[Int](mc.cols, mc.rows)
        for(r <- 0 to (mc.rows-1)) for(c <- 0 to (mc.cols-1)) {
            nmc(r,c) = (mc(r,c) - 1).abs
            mct(c, r) = mc(r,c)
        }

        val md = ma * mc 

        val i = CSCMatrix.zeros[Int](md.rows, md.cols)
        val ni = CSCMatrix.zeros[Int](md.rows, md.cols)
        for(r <- 0 to (md.rows-1)) for(c <- 0 to (md.cols-1)) {
            i(r,c) = if(md(r,c) == 0) 1 else 0
            ni(r, c) = if(md(r,c) == 0) 0 else 1
        }

        val inmc = i *:* nmc

        val man = CSCMatrix.zeros[Double](mc.cols, mc.cols)
        val mp = CSCMatrix.zeros[Double](mc.cols, mc.cols)

        ParVector.range(0, mc.cols).foreach( i => {
        //for(i <- 0 to (mc.cols-1)) {
            val ii = CSCMatrix.tabulate(mc.rows, mc.cols) { case (r, c) => inmc(r, c) * mc(r, i) }
            val tmp_mdsi = ma * ii
            val mbsi = DenseVector.zeros[Double](mc.cols)

            val mdsi = CSCMatrix.zeros[Double](mc.rows, mc.cols)
            val mmdsi = CSCMatrix.zeros[Double](mc.rows, mc.cols)
            val mvani = CSCMatrix.zeros[Double](mc.rows, mc.cols)
            val tmp_mpi1 = CSCMatrix.zeros[Double](mc.rows, mc.cols)
            val mcti = CSCMatrix.zeros[Double](1, mc.rows)

            for(r <- 0 to (mc.rows-1)) for(c <- 0 to (mc.cols-1)) {
                mdsi(r, c) = tmp_mdsi(r, c) * mc(r, i) * ni(r, c)
                mmdsi(r, c) = if(mdsi(r,c) != 0) 1.0 else 0
                mvani(r, c) = if(mdsi(r, c) != 0) if(mdsi(r, c) + md(r, c) != 0) (mdsi(r, c).asInstanceOf[Double] / (mdsi(r, c) + md(r, c))) - 0.5 else 0 else 0
                tmp_mpi1(r, c) = if(mvani(r, c) < 0) 1.0 else 0
                mbsi(c) += (if(mvani(r, c) != 0) 1 else 0)
                mcti(0, r) = mct(i, r).asInstanceOf[Double]
            }

            val mani1 = mcti * mvani
            val mani2 = mcti * mmdsi
            val mpi1 = mcti * tmp_mpi1

            for(j <- 0 to (mc.cols-1)) {
                man(i,j) = if(mani2(0, j) != 0) mani1(0, j)/mani2(0, j) else 0
                mp(i,j) = mpi1(0, j) / (if(mbsi(j) != 0) mbsi(j) else 1) * 100
            }
        })

        (man.toDenseMatrix, mp.toDenseMatrix)
    }

    def get_matrices[S, SourceID, DestID, CommT: ClassTag, NodeT: ClassTag](graph: Graph[S, SourceID, DestID], weight_att: Witness, comm_att: Witness)(
        implicit
        source_id: SelectField.Aux[graph.Repr, Witness.`'source`.T :: SourceID :: HNil, SourceID, NodeT],
        dest_id: SelectField.Aux[graph.Repr, Witness.`'dest`.T :: DestID :: HNil, DestID, NodeT],
        source_community: SelectField.Aux[graph.Repr, Witness.`'source`.T :: comm_att.T :: HNil, comm_att.T, CommT],
        dest_community: SelectField.Aux[graph.Repr, Witness.`'dest`.T :: comm_att.T :: HNil, comm_att.T, CommT],
        get_weight: SelectField.Aux[graph.Repr, Witness.`'edge`.T :: weight_att.T :: HNil, weight_att.T, Int],
    ): (CSCMatrix[Int], CSCMatrix[Int]) = {
        val comm_set: HashSet[CommT] = HashSet()
        val comm_map: HashMap[NodeT, CommT] = HashMap()
        val adj_map: HashMap[NodeT, HashMap[NodeT, Int]] = HashMap()
        graph.data.foreach(hlist => {
            val sid = source_id(hlist) ; val did = dest_id(hlist)
            val scomm = source_community(hlist); val dcomm = dest_community(hlist)
            
            comm_set += (scomm, dcomm)
            comm_map(sid) = scomm ; comm_map(did) = dcomm

            var source_map = adj_map.getOrElse(sid, HashMap())
            var current_weight = source_map.getOrElse(did, 0)
            source_map(did) = current_weight + get_weight(hlist)
            adj_map(sid) = source_map
        })
        val distinct_nodes = comm_map.keySet.toArray ; val nb_nodes = distinct_nodes.size
        val distinct_comm = comm_set.toArray ; val nb_comm = distinct_comm.size
        val adj_matrix = CSCMatrix.zeros[Int](nb_nodes, nb_nodes)
        val comm_matrix = CSCMatrix.zeros[Int](nb_nodes, nb_comm)
        for(i <- 0 to (nb_nodes-1)) {
            comm_matrix(i, distinct_comm.indexOf(comm_map(distinct_nodes(i)))) = 1
            adj_map.get(distinct_nodes(i)) match { case None => () ; case Some(source_map) => for(j <- 0 to (nb_nodes-1)) adj_matrix(i,j) = source_map.getOrElse(distinct_nodes(j), 0) }
        }
        (adj_matrix, comm_matrix)
    }

    private def to_adjacency_matrix[RowIndex: ClassTag, ColIndex, T](sparse_matrix: HashMap[RowIndex, HashMap[ColIndex, Int]])(
        implicit
        row_to_col: RowIndex =:= ColIndex
    ): CSCMatrix[Int] = {
        val full_keySet: HashSet[RowIndex] = HashSet()
        sparse_matrix.keys.foreach(rowkey => { full_keySet += rowkey ; sparse_matrix(rowkey).keys.foreach(colkey => full_keySet += row_to_col.flip(colkey)) })
        val full_keys = full_keySet.toArray
        val matrix_size = full_keys.size
        CSCMatrix.tabulate(matrix_size, matrix_size) { case (i, j) => (sparse_matrix get full_keys(i)) match { case None => 0 ; case Some(row) => row getOrElse (full_keys(j), 0) } }
    }

    private def to_community_matrix[RowIndex: ClassTag, ColIndex: ClassTag](sparse_matrix: HashMap[RowIndex, HashMap[ColIndex, Boolean]]): (CSCMatrix[Int], CSCMatrix[Int], CSCMatrix[Int]) = {
        var col_keySet: HashSet[ColIndex] = HashSet()
        val drow = new Array[RowIndex](sparse_matrix.keySet.size)
        var i = 0 ; sparse_matrix.keys.foreach(rowkey => {drow(i) = rowkey ; col_keySet ++= sparse_matrix(rowkey).keySet ; i += 1})
        val dcol = col_keySet.toArray

        val nb_row = i ; val nb_col = dcol.size

        val mc = CSCMatrix.zeros[Int](nb_row, nb_col) 
        val nmc = CSCMatrix.zeros[Int](nb_row, nb_col) 
        val mct = CSCMatrix.zeros[Int](nb_col, nb_row) 

        for(i <- 0 to (nb_row-1)) for(j <- 0 to (nb_col-1)) {
            val value = sparse_matrix.get(drow(i)) match {
                case None => 0
                case cols: Some[HashMap[ColIndex, Boolean]] => cols.get.get(dcol(j)) match { case v: Some[Boolean] => if(v.get) 1 else 0 ; case None => 0 }
            }
            mc(i, j) = value
            nmc(i, j) = (value - 1).abs
            mct(j, i) = value
        }
        
        (mc, nmc, mct)
    }

    val adj_toyex: HashMap[Int, HashMap[Int, Int]] = HashMap(1 -> HashMap(2 -> 3, 3 -> 2, 4 -> 4, 6 -> 5), 2 -> HashMap(3 -> 1, 6 -> 6), 3 -> HashMap(2 -> 5), 4 -> HashMap(1 -> 1, 5 -> 5, 6 -> 4), 6 -> HashMap(7 -> 5))
    val comm_toyex: HashMap[Int, HashMap[String, Boolean]] = HashMap(1 -> HashMap("C1" -> true), 2 -> HashMap("C1" -> true), 3 -> HashMap("C1" -> true), 4 -> HashMap("C2" -> true), 5 -> HashMap("C2" -> true), 6 -> HashMap("C2" -> true, "C3" -> true), 7 -> HashMap("C3" -> true))

    /* private def to_adjacency_matrix[RowIndex, ColIndex, T](sparse_matrix: HashMap[RowIndex, HashMap[ColIndex, Int]])(
        implicit
        row_to_col: RowIndex =:= ColIndex
    ): CSCMatrix[Int] = {
        var dcol_map: HashMap[ColIndex, Iterable[T]] = HashMap()
        sparse_matrix.keys.foreach(row => dcol_map.getOrElseUpdate(row_to_col(row), List()))
        sparse_matrix.values.foreach(col_map => col_map.keys.foreach(col => dcol_map.getOrElseUpdate(col, List())))
        val dcol = dcol_map.keys.toList
        val nb_col = dcol.size

        val values: List[Array[Int]] = dcol.map(row => sparse_matrix.get(row_to_col.flip(row)) match {
                case None => List.fill(nb_col)(0).toArray
                case cols: Some[HashMap[ColIndex, Int]] => dcol.map(col => cols.get.get(col) match { case v: Some[Int] => v.get ; case None => 0 }).toArray
            }
        )
        CSCMatrix(values:_*)
    } */
}