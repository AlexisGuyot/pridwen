package pridwen.operators.predefined

import shapeless.{HList, HNil}
import shapeless.labelled.{FieldType => Field}
import shapeless.ops.hlist.{Prepend}
import shapeless.ops.record.{Renamer}

import pridwen.support.functions.{getFieldValue}
import pridwen.support.{ReducePath}
import pridwen.models._
import pridwen.models.aux.{SelectAtt, SelectManyAtt, SelectSiblings, ValidModel, ReplaceAtt}
import pridwen.operators.aux.{CheckFName}

object join {
    def join[
        ML <: Model[_], MR <: Model[_],
        Path_To_Left_Key <: HList, Path_To_Right_Key <: HList,
        LK, RK, KT, RLK, RRK,
        Siblings_LK <: HList, Siblings_RK <: HList,
        LSchema <: HList, RSchema <: HList,
        New_Schema <: HList,
        MOut <: Model[_]
    ](
        ldataset: ML,
        rdataset: MR,
        lkey: Path_To_Left_Key,
        rkey: Path_To_Right_Key,
        mout: MOut
    )(
        implicit
        get_left_key: SelectAtt.Aux[ldataset.Repr, Path_To_Left_Key, LK, KT],
        get_right_key: SelectAtt.Aux[rdataset.Repr, Path_To_Right_Key, RK, KT],
        get_lk_siblings: SelectSiblings.Aux[ldataset.Repr, Path_To_Left_Key, Siblings_LK],
        get_rk_siblings: SelectSiblings.Aux[rdataset.Repr, Path_To_Right_Key, Siblings_RK],
        check_key_names: CheckFName.Aux[LK, RK, RLK, RRK],
        rename_lk: Renamer.Aux[Siblings_LK, LK, RLK, LSchema],
        rename_rk: Renamer.Aux[Siblings_RK, RK, RRK, RSchema],
        concat_siblings: Prepend.Aux[LSchema, RSchema, New_Schema],
        res_model: ValidModel[MOut, New_Schema, HNil]
    ): res_model.Out = join(ldataset, rdataset, lkey, rkey, mout, HNil)(get_left_key, get_right_key, get_lk_siblings, get_rk_siblings, check_key_names, rename_lk, rename_rk, concat_siblings, res_model)

    def join[
        ML <: Model[_], MR <: Model[_],
        Path_To_Left_Key <: HList, Path_To_Right_Key <: HList,
        LK, RK, KT, RLK, RRK,
        Siblings_LK <: HList, Siblings_RK <: HList,
        LSchema <: HList, RSchema <: HList,
        New_Schema <: HList,
        MOut <: Model[_], PMOut
    ](
        ldataset: ML,
        rdataset: MR,
        lkey: Path_To_Left_Key,
        rkey: Path_To_Right_Key,
        mout: MOut,
        pmout: PMOut
    )(
        implicit
        get_left_key: SelectAtt.Aux[ldataset.Repr, Path_To_Left_Key, LK, KT],
        get_right_key: SelectAtt.Aux[rdataset.Repr, Path_To_Right_Key, RK, KT],
        get_lk_siblings: SelectSiblings.Aux[ldataset.Repr, Path_To_Left_Key, Siblings_LK],
        get_rk_siblings: SelectSiblings.Aux[rdataset.Repr, Path_To_Right_Key, Siblings_RK],
        check_key_names: CheckFName.Aux[LK, RK, RLK, RRK],
        rename_lk: Renamer.Aux[Siblings_LK, LK, RLK, LSchema],
        rename_rk: Renamer.Aux[Siblings_RK, RK, RRK, RSchema],
        concat_siblings: Prepend.Aux[LSchema, RSchema, New_Schema],
        res_model: ValidModel[MOut, New_Schema, PMOut]
    ): res_model.Out = {
        val d = do_join(ldataset.data, rdataset.data, get_left_key.apply, get_right_key.apply, (lschema: ldataset.Repr, rschema: rdataset.Repr) =>
            concat_siblings(rename_lk(get_lk_siblings(lschema)), rename_rk(get_rk_siblings(rschema)))
        )
        res_model(d)
    }

    def join_in_left [
        ML <: Model[_], MR <: Model[_],
        Path_To_Left_Key <: HList, Reduced_LPath <: HList, Path_To_Right_Key <: HList,
        LK, RK, KT, RLK, RRK,
        Siblings_LK <: HList, Siblings_RK <: HList,
        LSchema <: HList, RSchema <: HList,
        New_Schema <: HList, New_LSchema <: HList
    ](
        ldataset: ML,
        rdataset: MR,
        lkey: Path_To_Left_Key,
        rkey: Path_To_Right_Key
    )(
        implicit
        get_left_key: SelectAtt.Aux[ldataset.Repr, Path_To_Left_Key, LK, KT],
        get_right_key: SelectAtt.Aux[rdataset.Repr, Path_To_Right_Key, RK, KT],
        get_lk_siblings: SelectSiblings.Aux[ldataset.Repr, Path_To_Left_Key, Siblings_LK],
        get_rk_siblings: SelectSiblings.Aux[rdataset.Repr, Path_To_Right_Key, Siblings_RK],
        check_key_names: CheckFName.Aux[LK, RK, RLK, RRK],
        rename_lk: Renamer.Aux[Siblings_LK, LK, RLK, LSchema],
        rename_rk: Renamer.Aux[Siblings_RK, RK, RRK, RSchema],
        concat_siblings: Prepend.Aux[LSchema, RSchema, New_Schema],
        reduced_left_path: ReducePath.Aux[Path_To_Left_Key, Reduced_LPath],
        update_lschema: ReplaceAtt.Aux[ldataset.Repr, Reduced_LPath, HNil, New_Schema, New_LSchema],
        res_model: ValidModel[ML, New_LSchema, HNil]
    ): res_model.Out = join_in_left(ldataset, HNil, rdataset, lkey, rkey)(get_left_key, get_right_key, get_lk_siblings, get_rk_siblings, check_key_names, rename_lk, rename_rk, concat_siblings, reduced_left_path, update_lschema, res_model)

    def join_in_left [
        ML <: Model[_], LMP, MR <: Model[_],
        Path_To_Left_Key <: HList, Reduced_LPath <: HList, Path_To_Right_Key <: HList,
        LK, RK, KT, RLK, RRK,
        Siblings_LK <: HList, Siblings_RK <: HList,
        LSchema <: HList, RSchema <: HList,
        New_Schema <: HList, New_LSchema <: HList
    ](
        ldataset: ML,
        left_mp: LMP,
        rdataset: MR,
        lkey: Path_To_Left_Key,
        rkey: Path_To_Right_Key
    )(
        implicit
        get_left_key: SelectAtt.Aux[ldataset.Repr, Path_To_Left_Key, LK, KT],
        get_right_key: SelectAtt.Aux[rdataset.Repr, Path_To_Right_Key, RK, KT],
        get_lk_siblings: SelectSiblings.Aux[ldataset.Repr, Path_To_Left_Key, Siblings_LK],
        get_rk_siblings: SelectSiblings.Aux[rdataset.Repr, Path_To_Right_Key, Siblings_RK],
        check_key_names: CheckFName.Aux[LK, RK, RLK, RRK],
        rename_lk: Renamer.Aux[Siblings_LK, LK, RLK, LSchema],
        rename_rk: Renamer.Aux[Siblings_RK, RK, RRK, RSchema],
        concat_siblings: Prepend.Aux[LSchema, RSchema, New_Schema],
        reduced_left_path: ReducePath.Aux[Path_To_Left_Key, Reduced_LPath],
        update_lschema: ReplaceAtt.Aux[ldataset.Repr, Reduced_LPath, HNil, New_Schema, New_LSchema],
        res_model: ValidModel[ML, New_LSchema, LMP]
    ): res_model.Out = {
        val d = do_join(ldataset.data, rdataset.data, get_left_key.apply, get_right_key.apply, (lschema: ldataset.Repr, rschema: rdataset.Repr) =>
            update_lschema(lschema, concat_siblings(rename_lk(get_lk_siblings(lschema)), rename_rk(get_rk_siblings(rschema))))
        )
        res_model(d)
    }

    def join_in_right [
        ML <: Model[_], MR <: Model[_],
        Path_To_Left_Key <: HList, Path_To_Right_Key <: HList, Reduced_RPath <: HList, 
        LK, RK, KT, RLK, RRK,
        Siblings_LK <: HList, Siblings_RK <: HList,
        LSchema <: HList, RSchema <: HList,
        New_Schema <: HList, New_RSchema <: HList
    ](
        ldataset: ML,
        rdataset: MR,
        lkey: Path_To_Left_Key,
        rkey: Path_To_Right_Key
    )(
        implicit
        get_left_key: SelectAtt.Aux[ldataset.Repr, Path_To_Left_Key, LK, KT],
        get_right_key: SelectAtt.Aux[rdataset.Repr, Path_To_Right_Key, RK, KT],
        get_lk_siblings: SelectSiblings.Aux[ldataset.Repr, Path_To_Left_Key, Siblings_LK],
        get_rk_siblings: SelectSiblings.Aux[rdataset.Repr, Path_To_Right_Key, Siblings_RK],
        check_key_names: CheckFName.Aux[LK, RK, RLK, RRK],
        rename_lk: Renamer.Aux[Siblings_LK, LK, RLK, LSchema],
        rename_rk: Renamer.Aux[Siblings_RK, RK, RRK, RSchema],
        concat_siblings: Prepend.Aux[LSchema, RSchema, New_Schema],
        reduced_right_path: ReducePath.Aux[Path_To_Right_Key, Reduced_RPath],
        update_rschema: ReplaceAtt.Aux[rdataset.Repr, Reduced_RPath, HNil, New_Schema, New_RSchema],
        res_model: ValidModel[MR, New_RSchema, HNil]
    ): res_model.Out = join_in_right(ldataset, rdataset, HNil, lkey, rkey)(get_left_key, get_right_key, get_lk_siblings, get_rk_siblings, check_key_names, rename_lk, rename_rk, concat_siblings, reduced_right_path, update_rschema, res_model)

    def join_in_right [
        ML <: Model[_], RMP, MR <: Model[_],
        Path_To_Left_Key <: HList, Path_To_Right_Key <: HList, Reduced_RPath <: HList,
        LK, RK, KT, RLK, RRK,
        Siblings_LK <: HList, Siblings_RK <: HList,
        LSchema <: HList, RSchema <: HList,
        New_Schema <: HList, New_RSchema <: HList
    ](
        ldataset: ML,
        rdataset: MR,
        right_mp: RMP,
        lkey: Path_To_Left_Key,
        rkey: Path_To_Right_Key
    )(
        implicit
        get_left_key: SelectAtt.Aux[ldataset.Repr, Path_To_Left_Key, LK, KT],
        get_right_key: SelectAtt.Aux[rdataset.Repr, Path_To_Right_Key, RK, KT],
        get_lk_siblings: SelectSiblings.Aux[ldataset.Repr, Path_To_Left_Key, Siblings_LK],
        get_rk_siblings: SelectSiblings.Aux[rdataset.Repr, Path_To_Right_Key, Siblings_RK],
        check_key_names: CheckFName.Aux[LK, RK, RLK, RRK],
        rename_lk: Renamer.Aux[Siblings_LK, LK, RLK, LSchema],
        rename_rk: Renamer.Aux[Siblings_RK, RK, RRK, RSchema],
        concat_siblings: Prepend.Aux[LSchema, RSchema, New_Schema],
        reduced_right_path: ReducePath.Aux[Path_To_Right_Key, Reduced_RPath],
        update_rschema: ReplaceAtt.Aux[rdataset.Repr, Reduced_RPath, HNil, New_Schema, New_RSchema],
        res_model: ValidModel[MR, New_RSchema, RMP]
    ): res_model.Out = {
        val d = do_join(ldataset.data, rdataset.data, get_left_key.apply, get_right_key.apply, (lschema: ldataset.Repr, rschema: rdataset.Repr) =>
            update_rschema(rschema, concat_siblings(rename_lk(get_lk_siblings(lschema)), rename_rk(get_rk_siblings(rschema))))
        )
        res_model(d)
    }

    private def do_join[L <: HList, R <: HList, LK, RK, V, O <: HList](
        left: List[L],
        right: List[R],
        left_key: L => Field[LK,V],
        right_key: R => Field[RK,V],
        join: (L, R) => O
    ): List[O] = {
        val d = scala.collection.mutable.ListBuffer.empty[O]
        left.foreach(lschema => right.foreach(rschema => 
            if(getFieldValue(left_key(lschema)) == getFieldValue(right_key(rschema))) join(lschema, rschema) +=: d
        ))
        d.to(List)
    }
}