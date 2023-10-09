import scala.collection.mutable.Map
import breeze.linalg._

import scala.reflect.{ClassTag}

object polarisation {
    def polarisation[NodeID, CommID](
        adj: Map[NodeID, Map[NodeID, Int]], 
        comm: Map[NodeID, Map[CommID, Boolean]]
    ) = {
        val Ma = mapToAdjMatrix(adj)
        val Mc = mapToMatrix(comm, (x: Boolean) => if(x) 1 else 0)
        
        val NMc =  new DenseMatrix(Mc.rows, Mc.cols, Mc.data.map((x: Int) => if(x == 0) 1 else 0))
        val McT = Mc.t

        val Md = Ma * Mc 

        val I = new DenseMatrix(Md.rows, Md.cols, Md.data.map((x: Int) => if(x == 0) 1 else 0))
        val NI = new DenseMatrix(Md.rows, Md.cols, Md.data.map((x: Int) => if(x == 0) 0 else 1))

        var Mani = new Array[DenseMatrix[Double]](Mc.cols)
        var Mpi = new Array[DenseMatrix[Double]](Mc.cols)
        for(i <- 0 to (Mc.cols-1)) {
            val Mci = tile(Mc(::,i), 1, Mc.cols)
            val Ii = (I *:* NMc) *:* Mci
            val Mdsi = (Ma * Ii) *:* Mci *:* NI
            val mMdsi = new DenseMatrix(Mdsi.rows, Mdsi.cols, Mdsi.data.map((x: Int) => if(x != 0) 1.0 else 0))
            val Mvani = DenseMatrix.tabulate(Mdsi.rows, Mdsi.cols){ case (r, c) => if(Mdsi(r, c) != 0) if(Mdsi(r, c) + Md(r, c) != 0) (Mdsi(r, c).asInstanceOf[Double] / (Mdsi(r, c) + Md(r, c))) - 0.5 else 0 else 0 }
            val McTi = McT(i,::).inner.map(v => v.asInstanceOf[Double]).toDenseMatrix
            val Mbsi = tile(sum(mMdsi, Axis._0), McTi.rows, Mvani.cols)
            val Mani1 = McTi * Mvani
            val Mani2 = McTi * mMdsi
            Mani(i) = DenseMatrix.tabulate(Mani1.rows, Mani1.cols){ case (r, c) => if(Mani2(r, c) != 0) Mani1(r,c)/Mani2(r,c) else 0 }
            val Mpi1 = McTi * (new DenseMatrix(Mvani.rows, Mvani.cols, Mvani.data.map((x: Double) => if(x < 0) 1.0 else 0)))
            Mpi(i) = DenseMatrix.tabulate(Mpi1.rows, Mpi1.cols){ case(r, c) => Mpi1(r, c) / (if(Mbsi(r, c) != 0) Mbsi(r, c) else 1) * 100 }
        }

        (
            Mani.tail.foldLeft(Mani.head){ (acc, row) => DenseMatrix.vertcat(acc, row) },
            Mpi.tail.foldLeft(Mpi.head){ (acc, row) => DenseMatrix.vertcat(acc, row) }
        )
    }

    private def mapToAdjMatrix[RowIndex, ColIndex, T](sparse_matrix: Map[RowIndex, Map[ColIndex, Int]])(
        implicit
        row_to_col: RowIndex =:= ColIndex
    ): DenseMatrix[Int] = {
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
        DenseMatrix(values:_*)
    }

    private def mapToMatrix[RowIndex, ColIndex, T](sparse_matrix: Map[RowIndex, Map[ColIndex, T]], to_int: T => Int): DenseMatrix[Int] = {
        var dcol_map: Map[ColIndex, Iterable[T]] = Map()
        sparse_matrix.values.foreach(col_map => col_map.keys.foreach(col => dcol_map.getOrElseUpdate(col, List())))
        val dcol = dcol_map.keys.toList
        val drow = sparse_matrix.keys.toList

        val values: List[Array[Int]] = drow.map(row => sparse_matrix.get(row) match {
                case None => Array()
                case cols: Some[Map[ColIndex, T]] => dcol.map(col => cols.get.get(col) match { case v: Some[T] => to_int(v.get) ; case None => 0 }).toArray
            }
        )
        DenseMatrix(values:_*)
    }
}