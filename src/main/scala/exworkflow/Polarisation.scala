import scala.collection.mutable.Map
import scala.collection.parallel.immutable.ParVector
import breeze.linalg._

import scala.reflect.{ClassTag}

object polarisation {
    def compute[NodeID: ClassTag, CommID: ClassTag](
        adj: Map[NodeID, Map[NodeID, Int]], 
        comm: Map[NodeID, Map[CommID, Boolean]]
    ) = {
        val ma = to_adjacency_matrix(adj)
        val (mc, nmc, mct) = to_community_matrix2(comm)

        val md = ma * mc 

        val i = CSCMatrix.tabulate(md.rows, md.cols){ case (r, c) => if(md(r,c) == 0) 1 else 0 }
        val ni = CSCMatrix.tabulate(md.rows, md.cols){ case (r, c) => if(md(r,c) == 0) 0 else 1 }

        val inmc = i *:* nmc

        val man = CSCMatrix.zeros[Double](mc.cols, mc.cols)
        val mp = CSCMatrix.zeros[Double](mc.cols, mc.cols)

        ParVector.range(0, mc.cols).foreach( i => {
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

    private def to_adjacency_matrix[RowIndex, ColIndex, T](sparse_matrix: Map[RowIndex, Map[ColIndex, Int]])(
        implicit
        row_to_col: RowIndex =:= ColIndex
    ): CSCMatrix[Int] = {
        var dcol_map: Map[ColIndex, Iterable[T]] = Map()
        sparse_matrix.keys.foreach(row => dcol_map.getOrElseUpdate(row_to_col(row), List()))
        sparse_matrix.values.foreach(col_map => col_map.keys.foreach(col => dcol_map.getOrElseUpdate(col, List())))
        val dcol = dcol_map.keys.toList
        val nb_col = dcol.size

        val values: List[Array[Int]] = dcol.map(row => sparse_matrix.get(row_to_col.flip(row)) match {
                case None => List.fill(nb_col)(0).toArray
                case cols: Some[Map[ColIndex, Int]] => dcol.map(col => cols.get.get(col) match { case v: Some[Int] => v.get ; case None => 0 }).toArray
            }
        )
        CSCMatrix(values:_*)
    }

    private def to_community_matrix2[RowIndex: ClassTag, ColIndex: ClassTag](sparse_matrix: Map[RowIndex, Map[ColIndex, Boolean]]): (CSCMatrix[Int], CSCMatrix[Int], CSCMatrix[Int]) = {
        var dcol_map: Map[ColIndex, Iterable[Boolean]] = Map()
        sparse_matrix.values.foreach(col_map => col_map.keys.foreach(col => dcol_map.getOrElseUpdate(col, List())))
        val dcol = dcol_map.keys.toArray
        val drow = sparse_matrix.keys.toArray

        val nb_row = drow.size ; val nb_col = dcol.size

        val mc = CSCMatrix.zeros[Int](nb_row, nb_col) 
        val nmc = CSCMatrix.zeros[Int](nb_row, nb_col) 
        val mct = CSCMatrix.zeros[Int](nb_col, nb_row) 

        for(i <- 0 to (nb_row-1)) for(j <- 0 to (nb_col-1)) {
            val value = sparse_matrix.get(drow(i)) match {
                case None => 0
                case cols: Some[Map[ColIndex, Boolean]] => cols.get.get(dcol(j)) match { case v: Some[Boolean] => if(v.get) 1 else 0 ; case None => 0 }
            }
            mc(i, j) = value
            nmc(i, j) = (value - 1).abs
            mct(j, i) = value
        }
        
        (mc, nmc, mct)
    }

    val adj_toyex: Map[Int, Map[Int, Int]] = Map(1 -> Map(2 -> 3, 3 -> 2, 4 -> 4, 6 -> 5), 2 -> Map(3 -> 1, 6 -> 6), 3 -> Map(2 -> 5), 4 -> Map(1 -> 1, 5 -> 5, 6 -> 4), 6 -> Map(7 -> 5))
    val comm_toyex: Map[Int, Map[String, Boolean]] = Map(1 -> Map("C1" -> true), 2 -> Map("C1" -> true), 3 -> Map("C1" -> true), 4 -> Map("C2" -> true), 5 -> Map("C2" -> true), 6 -> Map("C2" -> true, "C3" -> true), 7 -> Map("C3" -> true))
}