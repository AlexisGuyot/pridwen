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

        for(i <- 0 to Mc.cols) {
            
        }
    }

    /* private def mapToAdjMatrix[RowIndex, ColIndex, T](sparse_matrix: Map[RowIndex, Map[ColIndex, T]], to_int: T => Int)(
        implicit
        row_to_col: RowIndex =:= ColIndex
    ): DenseMatrix[Int] = {
        var nodes_map: Map[ColIndex, Iterable[T]] = Map()
        sparse_matrix.keys.foreach(source => nodes_map.getOrElseUpdate(row_to_col(source), List()))
        sparse_matrix.values.foreach(dest_map => dest_map.keys.foreach(dest => nodes_map.getOrElseUpdate(dest, List())))
        val nodes = nodes_map.keys.toList

        val nb_nodes = nodes.size
        val values = nodes.map(source => nodes.map(dest => sparse_matrix.getOrElse(row_to_col.flip(source), 0) match {
            case edge: Map[ColIndex, T] => edge.get(dest) match { case v: Some[T] => to_int(v.get) ; case None => 0 }
            case 0 => 0
        }))
        new DenseMatrix(nb_nodes, nb_nodes, values.flatten.toArray)
    } */

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