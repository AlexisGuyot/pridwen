package pridwen.operators.predefined

import shapeless.{HList, HNil, ::, Witness => W}
import shapeless.labelled.{FieldType => Field, field}

import pridwen.models._
import pridwen.models.aux.{SelectAtt}
import pridwen.support.functions.{getFieldValue, rename}

object ops {
    def constructGraph[
        M <: Model,
        Path_To_Source <: HList, Path_To_Source_Att <: HList, 
        Path_To_Dest <: HList, Path_To_Dest_Att <: HList,
        Path_To_Edges <: HList,
        Full_Path <: HList,
        SK, DK, NS
    ](
        m: M,
        source: Path_To_Source,
        dest: Path_To_Dest,
        source_att: Path_To_Source_Att,
        dest_att: Path_To_Dest_Att
    )(
        implicit
        sn1: SelectAtt.Aux[M#Schema, Path_To_Source, SK, NS],
        sn2: SelectAtt.Aux[M#Schema, Path_To_Dest, DK, NS],
    ) = (dataset: List[m.Schema]) => {
        val nodes_id = W('id)
        Graph(
            dataset
            .groupBy(hlist => (getFieldValue(sn1(hlist)), getFieldValue(sn2(hlist))))
            .mapValues(_.size)
            .map { case (key, value) => 
                (field[nodes_id.T](key._1)::HNil) :: 
                (field[nodes_id.T](key._2)::HNil) ::
                (field[W.`'weight`.T](value)::HNil) :: HNil
            }
            .toList,
            nodes_id
        )
    }
}