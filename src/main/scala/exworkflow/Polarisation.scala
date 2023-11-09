import pridwen.models.{Graph, GetNodes}
import pridwen.models.aux.{SelectField}
import pridwen.support.functions.{getFieldValue}

import shapeless.{Witness, ::, HNil}

import scala.collection.mutable.{Map, Set}
import scala.collection.immutable.{Queue}
import scala.collection.parallel.immutable.{ParVector, ParRange}
import scala.collection.parallel.mutable.{ParHashMap, ParArray}
import breeze.linalg.{Matrix, DenseMatrix, CSCMatrix, DenseVector, sum, Axis, csvwrite}
import breeze.numerics.abs

import scala.reflect.{ClassTag}

object polarisation {
    def compute[NodeID: ClassTag, CommID: ClassTag](
        adj: Map[NodeID, Map[NodeID, Int]], 
        comm: Map[NodeID, Map[CommID, Boolean]]
    ) = {
        import scala.collection.parallel.CollectionConverters._
        import time._

        val ma = adj
        println("Calcul mc")
        val mc: Map[NodeID, Map[CommID, Int]] = time { comm.par.map { case(r, row) => (r, row.par.map { case(c, _) => (c, 1) }.seq) }.seq }
        println("Calcul mct")
        val mct = time { transpose(mc) }

        println("Tri communautés par taille")
        var communities = time { mct.mapValues(_.size).toSeq.sortWith(_._2 > _._2).par }
        println("Calcul nmc")
        val nmc = time { val nmc: Map[NodeID, Map[CommID, Int]] = Map() 
        mc.par.foreach { case (r, row) => {
            val tmp_nmc: Map[CommID, Int] = Map()
            communities.foreach { case (c, _) => if(!(row contains c)) tmp_nmc(c) = 1 }
            if(!tmp_nmc.isEmpty) nmc(r) = tmp_nmc
        }}
        nmc }

        println("Calcul md")
        val md = time { product(ma, mc) }

        println("Calcul i, ni, inmc")
        val (i, ni, inmc) = time { val i: Map[NodeID, Map[CommID, Int]] = Map()
        val ni: Map[NodeID, Map[CommID, Int]] = Map()
        val inmc: Map[NodeID, Map[CommID, Int]] = Map()
        mc.par.foreach { case(r, _) => {
            val tmp_i: Map[CommID, Int] = Map() ; val tmp_ni: Map[CommID, Int] = Map() ; val tmp_inmc: Map[CommID, Int] = Map()
            communities.foreach { case (c, _) => {
                val tmp_ic = if(md.getOrElse(r, Map()).getOrElse(c, 0) == 0) 1 else 0
                if(tmp_ic == 1) tmp_i(c) = 1 else tmp_ni(c) = 1
                val tmp_inmcc = tmp_ic * nmc.getOrElse(r, Map()).getOrElse(c, 0)
                if(tmp_inmcc != 0) tmp_inmc(c) = tmp_inmcc
            }}
            if(!tmp_i.isEmpty) i(r) = tmp_i ; if(!tmp_ni.isEmpty) ni(r) = tmp_ni ; if(!tmp_inmc.isEmpty) inmc(r) = tmp_inmc
        }}    
        (i, ni, inmc) }

        val man: Map[CommID, Map[CommID, Double]] = Map()
        val mp: Map[CommID, Map[CommID, Double]] = Map()
        
        println("Calcul man et mp")
        time { communities.foreach { case (from, _) => {
            val ii: Map[NodeID, Map[CommID, Int]] = Map()
            println(s"Communauté ${from} : calcul de ii")
            time { inmc.par.foreach { case(r, row) =>  {
                val tmp_ii: Map[CommID, Int] = Map()
                row.par.map { case(c, value) => {
                    val tmp = value * mc.getOrElse(r, Map()).getOrElse(from, 0)
                    if(tmp != 0) tmp_ii(c) = tmp
                }}
                if(!tmp_ii.isEmpty) ii(r) = tmp_ii
            }} }
            println(s"Communauté ${from} : calcul de tmp_mdsi")
            val tmp_mdsi = time { product(ma, ii) }

            val mdsi: Map[NodeID, Map[CommID, Int]] = Map()
            val mmdsi: Map[NodeID, Map[CommID, Double]] = Map()
            val mvani: Map[NodeID, Map[CommID, Double]] = Map()
            val tmp_mpi1: Map[NodeID, Map[CommID, Double]] = Map()
            val mcti: Map[CommID, Map[NodeID, Double]] = Map(from -> Map())

            val mcr = mct.getOrElse(from, Map())

            val mani1: Map[CommID, Map[CommID, Double]] = Map(from -> Map())
            val mani2: Map[CommID, Map[CommID, Double]] = Map(from -> Map())
            val mpi1: Map[CommID, Map[CommID, Double]] = Map(from -> Map())

            println(s"Communauté ${from} : calcul des matrices intermédiaires")
            time { mc.par.foreach { case(r, row) => { 
                val n_mdsi: Map[CommID, Int] = Map() ; val n_mmdsi: Map[CommID, Double] = Map() ; val n_mvani: Map[CommID, Double] = Map() ; val n_tmp_mpi1: Map[CommID, Double] = Map()
                communities.par.foreach { case (c, _) => {
                    val mdsi_rc = tmp_mdsi.getOrElse(r, Map()).getOrElse(c, 0) * mcr.getOrElse(r, 0) * ni.getOrElse(r, Map()).getOrElse(c, 0)
                    if(mdsi_rc != 0) n_mdsi(c) = mdsi_rc
                    if(mdsi_rc != 0) n_mmdsi(c) = 1.0
                    val n_mvanic = if(mdsi_rc != 0) if(mdsi_rc + md.getOrElse(r, Map()).getOrElse(c, 0) != 0) (mdsi_rc.asInstanceOf[Double] / (mdsi_rc + md.getOrElse(r, Map()).getOrElse(c, 0))) - 0.5 else 0.0 else 0.0
                    if(n_mvanic != 0) { n_mvani(c) = n_mvanic ; mani1(from)(c) = mani1(from).getOrElse(c, 0.0) + n_mvanic ; mani2(from)(c) = mani2(from).getOrElse(c, 0.0) + 1.0 }
                    if(n_mvanic < 0) { n_tmp_mpi1(c) = 1.0 ; mpi1(from)(c) = mpi1(from).getOrElse(c, 0.0) + 1.0 }
                }}
                if(!n_mdsi.isEmpty) mdsi(r) = n_mdsi ; if(!n_mmdsi.isEmpty) mmdsi(r) = n_mmdsi ; if(!n_mvani.isEmpty) mvani(r) = n_mvani ; if(!n_tmp_mpi1.isEmpty) tmp_mpi1(r) = n_tmp_mpi1
                if(mct(from) contains r) mcti(from)(r) = mct(from)(r).asInstanceOf[Double]
            }} }

            println(s"Communauté ${from} : calcul de mbsi")
            val mbsi = time { transpose(mmdsi).par.map { case(r, row) => (r, row.values.foldLeft(0.0)(_ + _)) } }

            man(from) = Map() ; mp(from) = Map()
            println(s"Communauté ${from} : calcul de man et mp")
            time { communities.foreach { case (to, _) => {
                man(from)(to) = if(!mani1.isEmpty && !mani2.isEmpty && (mani2.head._2 contains to)) mani1.head._2.getOrElse(to, 0.0) / mani2.head._2(to) else 0
                mp(from)(to) = if(!mpi1.isEmpty) mpi1.head._2.getOrElse(to, 0.0) / (if(mbsi contains to) mbsi(to) else 1) * 100 else 0
            }} }
        }} }

        (man, mp)
    }

    def compute(ma: CSCMatrix[Int], mc: CSCMatrix[Int]) = {
        import scala.collection.parallel.CollectionConverters._
        import scala.language.postfixOps
        import time._

        //data.write_cscmatrix(ma, "/home/alexis/Documents/Tweets/GMerged/ma.csv")
        //data.write_cscmatrix(mc, "/home/alexis/Documents/Tweets/GMerged/mc.csv")

        println("Création de nmc et mct")
        //val (nmc, mct) = time { (CSCMatrix.tabulate(mc.rows, mc.cols) { case (r, c) => (mc(r,c) - 1).abs }, CSCMatrix.tabulate(mc.cols, mc.rows) { case (r, c) => mc(c, r) }) }
        val (nmc, mct) = time { (mc.map(v => if(v == 0) 1 else 0), mc.t) }

        println("Calcul md")
        val md = time { ma * mc }

        /* println("I et NI vides")
        val (i, ni) = (time { CSCMatrix.zeros[Int](md.rows, md.cols) }, time { CSCMatrix.zeros[Int](md.rows, md.cols) })

        println("Remplissage I et NI")
        //time { (0 to (md.rows-1) par).foreach(r => (0 to (md.cols-1) par).foreach(c =>  {
        time { for(r <- 0 to (md.rows-1)) for(c <- 0 to (md.cols-1)) {
            i(r,c) = if(md(r,c) == 0) 1 else 0
            ni(r, c) = if(md(r,c) == 0) 0 else 1
        //})) }
        } } */
        println("Création de i et ni") // Essayer de passer ni en DenseMatrix
        //val (i, ni) = time { (CSCMatrix.tabulate(md.rows, md.cols){ case (r,c) => if(md(r,c) == 0) 1 else 0 }, CSCMatrix.tabulate(md.rows, md.cols){ case (r,c) => if(md(r,c) == 0) 0 else 1 })}
        val (i, ni) = time { (md.map(v => if(v == 0) 1 else 0), md.map(v => if(v == 0) 0 else 1)) }

        println("Calcul inmc")
        val inmc = time { i *:* nmc }

        println("Man et Mp vides")
        val (man, mp) = (time { CSCMatrix.zeros[Double](mc.cols, mc.cols) }, time { CSCMatrix.zeros[Double](mc.cols, mc.cols) })
        
        /* println("Ecriture matrice mct")
        time { data.write_matrix(mct, "mct") }
        println("Ecriture matrices nmc, i, ni, inmc")
        time { data.write_matrices[Int](Array((nmc, "nmc"), (i, "i"), (ni, "ni"), (inmc, "inmc"))) } */

        println("Calcul lignes matrices ant et por")
        time { (0 to (mc.cols-1) par).foreach( i => {
        //time { (0 to (mc.cols-1)).foreach( i => {
        //for(i <- 0 to (mc.cols-1)) {
            println(s"Communauté ${i} : calcul de ii")
            val ii = time { CSCMatrix.tabulate(mc.rows, mc.cols) { case (r, c) => inmc(r, c) * mc(r, i) } }
            println(s"Communauté ${i} : calcul de tmp_mdsi")
            val tmp_mdsi = time { ma * ii }

            /* println(s"Communauté ${i} : initialisation matrices intermédiaires")
            val (mbsi, mdsi, mmdsi, mvani, tmp_mpi1, mcti) = time { (DenseVector.zeros[Double](mc.cols), 
                CSCMatrix.zeros[Double](mc.rows, mc.cols), 
                CSCMatrix.zeros[Double](mc.rows, mc.cols), 
                CSCMatrix.zeros[Double](mc.rows, mc.cols), 
                CSCMatrix.zeros[Double](mc.rows, mc.cols), 
                CSCMatrix.zeros[Double](1, mc.rows)) }


            println(s"Communauté ${i} : remplissage matrices intermédiaires")
            time { for(r <- 0 to (mc.rows-1)) for(c <- 0 to (mc.cols-1)) {
                mdsi(r, c) = tmp_mdsi(r, c) * mc(r, i) * ni(r, c)
                mmdsi(r, c) = if(mdsi(r,c) != 0) 1.0 else 0
                mvani(r, c) = if(mdsi(r, c) != 0) if(mdsi(r, c) + md(r, c) != 0) (mdsi(r, c).asInstanceOf[Double] / (mdsi(r, c) + md(r, c))) - 0.5 else 0 else 0
                tmp_mpi1(r, c) = if(mvani(r, c) < 0) 1.0 else 0
                mbsi(c) += (if(mvani(r, c) != 0) 1 else 0)
                mcti(0, r) = mct(i, r).asInstanceOf[Double]
            }} */

            println(s"Communauté ${i} : matrices intermédiaires")
            val mdsi = time { CSCMatrix.tabulate(mc.rows, mc.cols){ case(r,c) => tmp_mdsi(r, c) * mc(r, i) * ni(r, c) } }
            val mmdsi = time { mdsi.map(v => if(v != 0) 1.0 else 0) }
            val mvani = time { CSCMatrix.tabulate(mc.rows, mc.cols){ case(r,c) => if(mdsi(r, c) != 0) if(mdsi(r, c) + md(r, c) != 0) (mdsi(r, c).asInstanceOf[Double] / (mdsi(r, c) + md(r, c))) - 0.5 else 0 else 0 } }
            val tmp_mpi1 = time { mvani.map(v => if(v < 0) 1.0 else 0) }
            val mbsi = time { sum(mvani.map(v => if(v != 0) 1.0 else 0).toDenseMatrix, Axis._0) }
            val mcti = time { CSCMatrix.tabulate(1, mc.rows){ case(r,c) => mct(i,c).asInstanceOf[Double] } }

            /* println(s"Communauté ${i} : écriture matrice ii")
            time { data.write_matrix(ii, s"i${i}") }
            println(s"Communauté ${i} : écriture matrices mdsi, mmdsi, mvani")
            time { data.write_matrices[Double](Array((mdsi, s"mds${i}"), (mmdsi, s"mmds${i}"), (mvani, s"mvan${i}"))) } */

            println(s"Communauté ${i} : calcul de mani1")
            val mani1 = time { mcti * mvani }
            println(s"Communauté ${i} : calcul de mani2")
            val mani2 = time { mcti * mmdsi }
            println(s"Communauté ${i} : calcul de mpi1")
            val mpi1 = time { mcti * tmp_mpi1 }

            println(s"Communauté ${i} : remplissage de man et mp")
            time { for(j <- 0 to (mc.cols-1)) {
                man(i,j) = if(mani2(0, j) != 0) mani1(0, j)/mani2(0, j) else 0
                mp(i,j) = mpi1(0, j) / (if(mbsi(j) != 0) mbsi(j) else 1) * 100
            } }
        }) }

        println("Conversion en matrices pleines")
        (time { man.toDenseMatrix }, time { mp.toDenseMatrix })
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
        import scala.language.postfixOps
        import time._

        val comm_set: Set[CommT] = Set()
        val comm_map: Map[NodeT, CommT] = Map()
        val rev_adj_map: Map[NodeT, Map[NodeT, Int]] = Map()

        println("Construction hashmaps")
        time { graph.data.foreach(hlist => {
            val sid = source_id(hlist) ; val did = dest_id(hlist)
            val scomm = source_community(hlist); val dcomm = dest_community(hlist)
            
            comm_set += (scomm, dcomm)
            comm_map(sid) = scomm 
            comm_map(did) = dcomm

            val dest_map = rev_adj_map.getOrElse(did, Map())
            dest_map(sid) = dest_map.getOrElse(sid, 0) + get_weight(hlist)
            rev_adj_map(did) = dest_map
            rev_adj_map(sid) = rev_adj_map.getOrElse(sid, Map())
        }) }

        val nodes = rev_adj_map.keySet.toIndexedSeq ; val nb_nodes = nodes.length
        val communities = comm_set.toIndexedSeq ; val nb_comm = communities.length
        println("Construction matrices")
        val (_, nodeID_to_ind) = time { nodes.foldLeft((0, Map[NodeT, Int]())){ case ((i, res), node) => (i+1, res += (node -> i)) } }
        val (data, indices, indptr, _, _) = time { rev_adj_map.foldLeft((Queue[Int](), Queue[Int](), Queue[Int](), 0, 0)){ case ((d, ind, iptr, i, j), (dest, in)) => {
            val keys = in.keys
            (
                d :++ in.values,
                ind :++ keys.map(k => nodeID_to_ind(k)),
                iptr :+ j,
                i+1,
                j+keys.size
            )
        }} }
        val adj_matrix = time { new CSCMatrix(data.toArray, nb_nodes, nb_nodes, (indptr :+ nb_nodes).toArray, indices.toArray) }
        val comm_matrix = time { CSCMatrix.tabulate(nb_nodes, nb_comm) { case (i, j) => if(comm_map(nodes(i)) == communities(j)) 1 else 0 } }

        (adj_matrix, comm_matrix)
    }

    def elwise_product[Row, Col, T](lm: Map[Row, Map[Col, T]], rm: Map[Row, Map[Col, T]])(
        implicit
        num: Numeric[T]
    ): Map[Row, Map[Col, T]] = {
        import Numeric.Implicits._
        import scala.collection.parallel.CollectionConverters._

        var res: Map[Row, Map[Col, T]] = Map()
        lm.par.foreach { case (i, row) => {
            val in_rm = rm.getOrElse(i, Map())
            if(!in_rm.isEmpty) {
            val m: Map[Col, T] = Map()
            row.par.foreach { case (j, lvalue) => {
                val rvalue = in_rm.getOrElse(j, num.zero)
                if(rvalue != 0) m(j) = num.times(lvalue, rvalue)
            } 
            res(i) = m
        } } } }
        res
    }

    def product[LRow, CID, RCol, T](lm: Map[LRow, Map[CID, T]], rm: Map[CID, Map[RCol, T]])(
        implicit
        num: Numeric[T]
    ): Map[LRow, Map[RCol, T]] = {
        import Numeric.Implicits._
        import scala.collection.parallel.CollectionConverters._

        //var inv_rm: Map[RCol, Map[CID, T]] = Map()
        //rm.foreach { case(i, row) => row.foreach { case(j, value) => inv_rm(j) = inv_rm.getOrElse(j, Map()) + (i -> value) }}
        val inv_rm = transpose(rm)

        var res: Map[LRow, Map[RCol, T]] = Map()
        lm.par.foreach { case (i, lrow) => {
        //lm.foreach { case (i, lrow) => {
            val m: Map[RCol, T] = Map()
            inv_rm.par.foreach { case(j, rcol) => {
            //inv_rm.foreach { case(j, rcol) => {
                var s = num.zero
                lrow.keySet.union(rcol.keySet).par.foreach(x => 
                //lrow.keySet.union(rcol.keySet).foreach(x => 
                    s = num.plus(s, num.times(lrow.getOrElse(x, num.zero), rm.getOrElse(x, Map()).getOrElse(j, num.zero)))
                )
                if(num.compare(s, num.zero) != 0) m(j) = s
            }}
            if(!m.isEmpty) res(i) = m
        }}
        res
    }

    def transpose[Row, Col, T](m: Map[Row, Map[Col, T]]): Map[Col, Map[Row, T]] = {
        import scala.collection.parallel.CollectionConverters._
        var res: ParHashMap[Col, Map[Row, T]] = ParHashMap()
        m.par.foreach { case(i, row) => row.par.foreach { case(j, value) => res(j) = res.getOrElse(j, Map()) + (i -> value) }}
        res.to(Map)
    }


    private def to_adjacency_matrix[RowIndex: ClassTag, ColIndex, T](sparse_matrix: Map[RowIndex, Map[ColIndex, Int]])(
        implicit
        row_to_col: RowIndex =:= ColIndex
    ): CSCMatrix[Int] = {
        val full_keySet: Set[RowIndex] = Set()
        sparse_matrix.keys.foreach(rowkey => { full_keySet += rowkey ; sparse_matrix(rowkey).keys.foreach(colkey => full_keySet += row_to_col.flip(colkey)) })
        val full_keys = full_keySet.toArray
        val matrix_size = full_keys.size
        CSCMatrix.tabulate(matrix_size, matrix_size) { case (i, j) => (sparse_matrix get full_keys(i)) match { case None => 0 ; case Some(row) => row getOrElse (full_keys(j), 0) } }
    }

    private def to_community_matrix[RowIndex: ClassTag, ColIndex: ClassTag](sparse_matrix: Map[RowIndex, Map[ColIndex, Boolean]]): (CSCMatrix[Int], CSCMatrix[Int], CSCMatrix[Int]) = {
        var col_keySet: Set[ColIndex] = Set()
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

    val breeze_adj_toyex = CSCMatrix((0, 3, 2, 4, 0, 5, 0), (0, 0, 1, 0, 0, 6, 0), (0, 0, 5, 0, 0, 0, 0), (1, 0, 0, 0, 5, 4, 0), (0, 0, 0, 0, 0, 0, 0), (0, 0, 0, 0, 0, 0, 5), (0, 0, 0, 0, 0, 0, 0))
    val breeze_comm_toyex = CSCMatrix((1, 0, 0), (1, 0, 0), (1, 0, 0), (0, 1, 0), (0, 1, 0), (0, 1, 1), (0, 0, 1))

    /* private def to_adjacency_matrix[RowIndex, ColIndex, T](sparse_matrix: Map[RowIndex, Map[ColIndex, Int]])(
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
    } */
}