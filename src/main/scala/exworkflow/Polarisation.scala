import pridwen.models.{Graph, GetNodes}
import pridwen.models.aux.{SelectField}

import shapeless.{Witness, ::, HNil}

import scala.collection.mutable.{HashMap, HashSet}
import scala.collection.parallel.immutable.{ParVector, ParRange}
import scala.collection.parallel.mutable.{ParHashMap}
import breeze.linalg.{Matrix, DenseMatrix, CSCMatrix, DenseVector}

import scala.reflect.{ClassTag}

object polarisation {
    def compute[NodeID: ClassTag, CommID: ClassTag](
        adj: HashMap[NodeID, HashMap[NodeID, Int]], 
        comm: HashMap[NodeID, HashMap[CommID, Boolean]]
    ) = {
        import scala.collection.parallel.CollectionConverters._
        import time._

        val ma = adj
        val mc: HashMap[NodeID, HashMap[CommID, Int]] = time { comm.par.map { case(r, row) => (r, row.par.map { case(c, _) => (c, 1) }.seq) }.seq }
        val mct = time { transpose(mc) }

        var communities = time { mct.mapValues(_.size).toSeq.sortWith(_._2 > _._2).par }
        val nmc = time { val nmc: HashMap[NodeID, HashMap[CommID, Int]] = HashMap() 
        mc.par.foreach { case (r, row) => {
            val tmp_nmc: HashMap[CommID, Int] = HashMap()
            communities.foreach { case (c, _) => if(!(row contains c)) tmp_nmc(c) = 1 }
            if(!tmp_nmc.isEmpty) nmc(r) = tmp_nmc
        }}
        nmc }

        val md = time { product(ma, mc) }

        val (i, ni, inmc) = time { val i: HashMap[NodeID, HashMap[CommID, Int]] = HashMap()
        val ni: HashMap[NodeID, HashMap[CommID, Int]] = HashMap()
        val inmc: HashMap[NodeID, HashMap[CommID, Int]] = HashMap()
        mc.par.foreach { case(r, _) => {
            val tmp_i: HashMap[CommID, Int] = HashMap() ; val tmp_ni: HashMap[CommID, Int] = HashMap() ; val tmp_inmc: HashMap[CommID, Int] = HashMap()
            communities.foreach { case (c, _) => {
                val tmp_ic = if(md.getOrElse(r, HashMap()).getOrElse(c, 0) == 0) 1 else 0
                if(tmp_ic == 1) tmp_i(c) = 1 else tmp_ni(c) = 1
                val tmp_inmcc = tmp_ic * nmc.getOrElse(r, HashMap()).getOrElse(c, 0)
                if(tmp_inmcc != 0) tmp_inmc(c) = tmp_inmcc
            }}
            if(!tmp_i.isEmpty) i(r) = tmp_i ; if(!tmp_ni.isEmpty) ni(r) = tmp_ni ; if(!tmp_inmc.isEmpty) inmc(r) = tmp_inmc
        }}    
        (i, ni, inmc) }

        val man: HashMap[CommID, HashMap[CommID, Double]] = HashMap()
        val mp: HashMap[CommID, HashMap[CommID, Double]] = HashMap()
        
        /* time { communities.foreach { case (from, _) => {
            val ii: HashMap[NodeID, HashMap[CommID, Int]] = HashMap()
            inmc.par.foreach { case(r, row) =>  {
                val tmp_ii: HashMap[CommID, Int] = HashMap()
                row.par.map { case(c, value) => {
                    val tmp = value * mc.getOrElse(r, HashMap()).getOrElse(from, 0)
                    if(tmp != 0) tmp_ii(c) = tmp
                }}
                if(!tmp_ii.isEmpty) ii(r) = tmp_ii
            }}
            val tmp_mdsi = product(ma, ii)

            val mdsi: HashMap[NodeID, HashMap[CommID, Int]] = HashMap()
            val mmdsi: HashMap[NodeID, HashMap[CommID, Double]] = HashMap()
            val mvani: HashMap[NodeID, HashMap[CommID, Double]] = HashMap()
            val tmp_mpi1: HashMap[NodeID, HashMap[CommID, Double]] = HashMap()
            val mcti: HashMap[CommID, HashMap[NodeID, Double]] = HashMap(from -> HashMap())

            val mcr = mct.getOrElse(from, HashMap())

            val mani1: HashMap[CommID, HashMap[CommID, Double]] = HashMap(from -> HashMap())
            val mani2: HashMap[CommID, HashMap[CommID, Double]] = HashMap(from -> HashMap())
            val mpi1: HashMap[CommID, HashMap[CommID, Double]] = HashMap(from -> HashMap())

            mc.par.foreach { case(r, row) => { 
                val n_mdsi: HashMap[CommID, Int] = HashMap() ; val n_mmdsi: HashMap[CommID, Double] = HashMap() ; val n_mvani: HashMap[CommID, Double] = HashMap() ; val n_tmp_mpi1: HashMap[CommID, Double] = HashMap()
                communities.par.foreach { case (c, _) => {
                    val mdsi_rc = tmp_mdsi.getOrElse(r, HashMap()).getOrElse(c, 0) * mcr.getOrElse(r, 0) * ni.getOrElse(r, HashMap()).getOrElse(c, 0)
                    if(mdsi_rc != 0) n_mdsi(c) = mdsi_rc
                    if(mdsi_rc != 0) n_mmdsi(c) = 1.0
                    val n_mvanic = if(mdsi_rc != 0) if(mdsi_rc + md.getOrElse(r, HashMap()).getOrElse(c, 0) != 0) (mdsi_rc.asInstanceOf[Double] / (mdsi_rc + md.getOrElse(r, HashMap()).getOrElse(c, 0))) - 0.5 else 0.0 else 0.0
                    if(n_mvanic != 0) { n_mvani(c) = n_mvanic ; mani1(from)(c) = mani1(from).getOrElse(c, 0.0) + n_mvanic ; mani2(from)(c) = mani2(from).getOrElse(c, 0.0) + 1.0 }
                    if(n_mvanic < 0) { n_tmp_mpi1(c) = 1.0 ; mpi1(from)(c) = mpi1(from).getOrElse(c, 0.0) + 1.0 }
                }}
                if(!n_mdsi.isEmpty) mdsi(r) = n_mdsi ; if(!n_mmdsi.isEmpty) mmdsi(r) = n_mmdsi ; if(!n_mvani.isEmpty) mvani(r) = n_mvani ; if(!n_tmp_mpi1.isEmpty) tmp_mpi1(r) = n_tmp_mpi1
                if(mct(from) contains r) mcti(from)(r) = mct(from)(r).asInstanceOf[Double]
            }}

            val mbsi = transpose(mmdsi).par.map { case(r, row) => (r, row.values.foldLeft(0.0)(_ + _)) }

            man(from) = HashMap() ; mp(from) = HashMap()
            communities.foreach { case (to, _) => {
                man(from)(to) = if(!mani1.isEmpty && !mani2.isEmpty && (mani2.head._2 contains to)) mani1.head._2.getOrElse(to, 0.0) / mani2.head._2(to) else 0
                mp(from)(to) = if(!mpi1.isEmpty) mpi1.head._2.getOrElse(to, 0.0) / (if(mbsi contains to) mbsi(to) else 1) * 100 else 0
            }}
        }} } */

        (man, mp)
    }

    def compute(ma: Matrix[Int], mc: Matrix[Int]) = {
        val nmc = CSCMatrix.zeros[Int](mc.rows, mc.cols)
        val mct = CSCMatrix.zeros[Int](mc.cols, mc.rows)
        /* for(r <- 0 to (mc.rows-1)) for(c <- 0 to (mc.cols-1)) {
            nmc(r,c) = (mc(r,c) - 1).abs
            mct(c, r) = mc(r,c)
        } */
        ParVector.range(0, mc.rows).foreach(r => ParVector.range(0, mc.cols).foreach(c =>  {
            nmc(r,c) = (mc(r,c) - 1).abs
            mct(c, r) = mc(r,c)
        }))

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

        /* data.write_matrix(nmc, "nmc")
        data.write_matrix(mct, "mct")
        data.write_matrix(i, "i")
        data.write_matrix(ni, "ni")
        data.write_matrix(inmc, "inmc") */

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

            /* data.write_matrix(ii, s"i${i}")
            data.write_matrix(mdsi, s"mds${i}")
            data.write_matrix(mmdsi, s"mmds${i}")
            data.write_matrix(mvani, s"mvan${i}")
            data.write_matrix(mcti, s"mct${i}") */

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
        import scala.collection.parallel.CollectionConverters._
        import time._

        val comm_set: HashSet[CommT] = HashSet()
        val comm_map: HashMap[NodeT, CommT] = HashMap()
        val adj_map: HashMap[NodeT, HashMap[NodeT, Int]] = HashMap()
        graph.data.foreach(hlist => {
            val sid = source_id(hlist) ; val did = dest_id(hlist)
            val scomm = source_community(hlist); val dcomm = dest_community(hlist)
            
            comm_set += (scomm, dcomm)
            comm_map(sid) = scomm 
            comm_map(did) = dcomm

            var source_map = adj_map.getOrElse(sid, HashMap())
            var current_weight = source_map.getOrElse(did, 0)
            source_map(did) = current_weight + get_weight(hlist)
            adj_map(sid) = source_map
            adj_map(did) = adj_map.getOrElse(did, HashMap())
        })
        val distinct_nodes = adj_map.keySet.toArray ; val nb_nodes = distinct_nodes.size
        val distinct_comm = comm_set.toArray ; val nb_comm = distinct_comm.size
        val adj_matrix = CSCMatrix.zeros[Int](nb_nodes, nb_nodes)
        val comm_matrix = CSCMatrix.zeros[Int](nb_nodes, nb_comm)
        for(i <- 0 to (nb_nodes-1)) {
            comm_matrix(i, distinct_comm.indexOf(comm_map(distinct_nodes(i)))) = 1
            val source_map = adj_map.getOrElse(distinct_nodes(i), HashMap())
            if(!source_map.isEmpty) for(j <- 0 to (nb_nodes-1)) adj_matrix(i,j) = source_map.getOrElse(distinct_nodes(j), 0) else ()
        }

        (adj_matrix, comm_matrix)
    }

    def elwise_product[Row, Col, T](lm: HashMap[Row, HashMap[Col, T]], rm: HashMap[Row, HashMap[Col, T]])(
        implicit
        num: Numeric[T]
    ): HashMap[Row, HashMap[Col, T]] = {
        import Numeric.Implicits._
        import scala.collection.parallel.CollectionConverters._

        var res: HashMap[Row, HashMap[Col, T]] = HashMap()
        lm.par.foreach { case (i, row) => {
            val in_rm = rm.getOrElse(i, HashMap())
            if(!in_rm.isEmpty) {
            val m: HashMap[Col, T] = HashMap()
            row.par.foreach { case (j, lvalue) => {
                val rvalue = in_rm.getOrElse(j, num.zero)
                if(rvalue != 0) m(j) = num.times(lvalue, rvalue)
            } 
            res(i) = m
        } } } }
        res
    }

    def product[LRow, CID, RCol, T](lm: HashMap[LRow, HashMap[CID, T]], rm: HashMap[CID, HashMap[RCol, T]])(
        implicit
        num: Numeric[T]
    ): HashMap[LRow, HashMap[RCol, T]] = {
        import Numeric.Implicits._
        import scala.collection.parallel.CollectionConverters._

        //var inv_rm: HashMap[RCol, HashMap[CID, T]] = HashMap()
        //rm.foreach { case(i, row) => row.foreach { case(j, value) => inv_rm(j) = inv_rm.getOrElse(j, HashMap()) + (i -> value) }}
        val inv_rm = transpose(rm)

        var res: HashMap[LRow, HashMap[RCol, T]] = HashMap()
        lm.par.foreach { case (i, lrow) => {
            val m: HashMap[RCol, T] = HashMap()
            inv_rm.par.foreach { case(j, rcol) => {
                var s = num.zero
                lrow.keySet.par.union(rcol.keySet).foreach(x => 
                    s = num.plus(s, num.times(lrow.getOrElse(x, num.zero), rm.getOrElse(x, HashMap()).getOrElse(j, num.zero)))
                )
                if(num.compare(s, num.zero) != 0) m(j) = s
            }}
            if(!m.isEmpty) res(i) = m
        }}
        res
    }

    def transpose[Row, Col, T](m: HashMap[Row, HashMap[Col, T]]): HashMap[Col, HashMap[Row, T]] = {
        import scala.collection.parallel.CollectionConverters._
        var res: ParHashMap[Col, HashMap[Row, T]] = ParHashMap()
        m.par.foreach { case(i, row) => row.par.foreach { case(j, value) => res(j) = res.getOrElse(j, HashMap()) + (i -> value) }}
        res.to(HashMap)
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