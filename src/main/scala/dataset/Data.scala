package pridwen.dataset

import pridwen.types.models._
import pridwen.types.opschema._
import pridwen.types.support.{DeepLabelledGeneric => LabelledGeneric, DecompPath}

import ColumnOps.{FOperator, AOperator, OOperator, AggOperator}

import shapeless.{HList, HNil, ::, Witness}
import shapeless.labelled.{FieldType}
import shapeless.ops.hlist.Selector

import org.apache.spark.sql.{Dataset, DataFrame, Encoder, Row, Column}
import org.apache.spark.sql.functions.{col, lit, struct, typedLit}

import scala.reflect.runtime.universe.TypeTag
import scala.annotation.implicitNotFound


trait Data[M <: Model, S <: HList] {
    type DST
    val data: Dataset[DST]
    def toDF(): DataFrame = data.toDF
    def toDS = new {
        def apply: Dataset[DST] = data
        def withSchema[CS <: Product](implicit toHList: LabelledGeneric.Aux[CS,S], enc: Encoder[CS]): Dataset[CS] = data.as[CS]
    }

    // show
    def show(numRows: Int, truncate: Int, vertical: Boolean): Unit = data.show(numRows, truncate, vertical)
    def show(numRows: Int, truncate: Int): Unit = data.show(numRows, truncate)
    def show(numRows: Int, truncate: Boolean): Unit = data.show(numRows, truncate)
    def show(truncate: Boolean): Unit = data.show(truncate)
    def show: Unit = data.show()
    def show(numRows: Int): Unit = data.show(numRows)

    // describe
    private def inner_describe(name: String, showData: Boolean, printSchema: Schema.AsString[S], printModel: Model.AsString[M]): Unit = {
        println(s"\n============= ${name}\n")
        println(s"Model: ${printModel()}\n")
        println(s"Schema: \n${printSchema()}")
        if(showData) data.show
        println("=======================================")
    }
    def describe(implicit printSchema: Schema.AsString[S], printModel: Model.AsString[M]) = inner_describe("Dataset", false, printSchema, printModel)
    def describe(name: String)(implicit printSchema: Schema.AsString[S], printModel: Model.AsString[M]) = inner_describe(name, false, printSchema, printModel)
    def describe(showData: Boolean)(implicit printSchema: Schema.AsString[S], printModel: Model.AsString[M]) = inner_describe("Dataset", showData, printSchema, printModel)
    def describe(name: String, showData: Boolean)(implicit printSchema: Schema.AsString[S], printModel: Model.AsString[M]) = inner_describe(name, showData, printSchema, printModel)

    // count
    def count: Long = data.count
}

object Data {
    def apply[S, HS <: HList](ds: Dataset[S])(implicit toHList: LabelledGeneric.Aux[S, HS]) = new {
        def as[M <: Model](implicit isValid: Model.As[HS, M]) = new Data[M, HS] { type DST = S ; val data = ds }
    }

    def apply[M <: Model] = new {
        def apply[S, HS <: HList](ds: Dataset[S])(implicit toHList: LabelledGeneric.Aux[S, HS], isValid: Model.As[HS, M]) 
            = new Data[M, HS] { type DST = S ; val data = ds }
    }

    implicit def dataOps[M <: Model, S <: HList](d: Data[M,S]): DataOps[M,S] = new DataOps[M,S](d)
}

final class DataOps[M <: Model, S <: HList](d: Data[M, S]) {
    // ============ Change model

    def as[NM <: Model](implicit isValid: Model.As[S, NM]): Data[NM, S] = new Data[NM, S] {
        type DST = d.DST
        val data = d.data
    }

    // ============ Project attribute(s)

    // Project one attribute (Path with alias)
    def select[PW <: HList, NN <: Symbol, PS <: HList, FT](path: Path.As.Aux[PW, NN, PS])(implicit s: SelectField.As.Aux[S, PS, NN, FT], alias: Witness.Aux[NN]): Data[M, FieldType[NN, FT] :: HNil] = new Data[M, FieldType[NN, FT] :: HNil] {
        type DST = Row
        val data = d.data.select(d.data.col(path.asString).as(alias.value.name))
    }

    // Project multiple attributes (MultiplePaths)
    def select[MPW <: HList, MPS <: HList, NS <: HList](paths: MultiplePaths.Aux[MPW, MPS])(implicit s: SelectMany.Aux[S, MPS, NS]): Data[M, NS] = new Data[M, NS] {
        type DST = Row
        val data = d.data.select(paths.toColumns: _*)
    }

    // ============ Select rows

    def filter[O <: FOperator, I](f: FilterOps[O,I])(implicit filter: FilterOps.Compute[S, O, I]): Data[M, S] = new Data[M, S] {
        type DST = d.DST
        val data = d.data.filter(filter.toSparkColumn)
    }

    def filter[CPW <: HList, CPS <: HList, P <: HList, FN <: Symbol, FT, NS <: HList, F](
        basedOn: MultiplePaths.Aux[CPW,CPS], f: F
    )(
        implicit
        udf: FuncOnSchema.Aux[S, CPS, F, Boolean]
    ): Data[M, S] = new Data[M, S] {
        type DST = d.DST
        val data = d.data.filter(udf(f)(basedOn.toColumns:_*))
    }

    def filter[MPW <: HList, MPS <: HList, P <: HList, FN <: Symbol, FT, NS <: HList, F](
        f: F
    )(
        implicit
        s: SelectAll.Aux[S, MPW],
        basedOn: MultiplePaths.Aux[MPW, MPS],
        udf: FuncOnSchema.Aux[S, MPS, F, Boolean]
    ): Data[M, S] = new Data[M, S] {
        type DST = d.DST
        val data = d.data.filter(udf(f)(basedOn.toColumns:_*))
    }

    // ============ Add attribute

    // New value from default value
    def add[PW <: HList, PS <: HList, P <: HList, FN <: Symbol, FT: TypeTag, NS <: HList](
        field: Path.Aux[PW, PS], default: FT
    )(
        implicit
        p: DecompPath.Aux[PS, P, FN],
        a: AddField.Aux[S, P, FN, FT, NS]
    ) = inner_add[NS, PW, PS](field, typedLit(default))

    // New value by mapping
    def add[FPW <: HList, FPS <: HList, CPW <: HList, CPS <: HList, P <: HList, FN <: Symbol, FT, NS <: HList, F](
        field: Path.Aux[FPW,FPS], basedOn: MultiplePaths.Aux[CPW,CPS], f: F
    )(
        implicit
        udf: FuncOnSchema.Aux[S, CPS, F, FT],
        p: DecompPath.Aux[FPS, P, FN],
        a: AddField.Aux[S, P, FN, FT, NS]
    ) = inner_add[NS, FPW, FPS](field, udf(f)(basedOn.toColumns:_*))

    // New value by mapping all columns
    def add[FPW <: HList, FPS <: HList, MPW <: HList, MPS <: HList, P <: HList, FN <: Symbol, FT, NS <: HList, F](
        field: Path.Aux[FPW,FPS], all: "*", f: F
    )(
        implicit
        s: SelectAll.Aux[S, MPW],
        basedOn: MultiplePaths.Aux[MPW, MPS],
        udf: FuncOnSchema.Aux[S, MPS, F, FT],
        p: DecompPath.Aux[FPS, P, FN],
        a: AddField.Aux[S, P, FN, FT, NS]
    ) = inner_add[NS, FPW, FPS](field, udf(f)(basedOn.toColumns:_*))

    // New value using spark column operators
    def add[FPW <: HList, FPS <: HList, O <: AOperator, I, P <: HList, FN <: Symbol, FT, NS <: HList](
        field: Path.Aux[FPW,FPS], f: AddOps[O,I]
    )(
        implicit 
        p: DecompPath.Aux[FPS, P, FN],
        op: AddOps.Compute.Aux[S, O, I, FT],
        add: AddField.Aux[S, P, FN, FT, NS]
    ) = inner_add[NS, FPW, FPS](field, op.toSparkColumn)

    private def inner_add[NS <: HList, PW <: HList, PS <: HList](
        field: Path.Aux[PW, PS],
        columns: Column
    ) = new {
        private def common[A <: Model, B <: HList]: Data[A,B] = new Data[A,B]{
            type DST = Row
            val data = {
                val path = field.asString.split("\\.")
                if(path.size >= 2) {
                    val (subpath, fname) = (path.slice(0,path.size-1).mkString("."), path.last)
                    d.data.withColumn(subpath, struct(col(s"$subpath.*"), columns.as(fname)))
                } else {
                    d.data.withColumn(field.asString, columns)
                }
            }
        }

        // Without model change
        def keepModel(implicit isValid: Model.As[NS, M]): Data[M, NS] = common[M, NS]

        // With model change
        def changeModel[NM <: Model](implicit isValid: Model.As[NS, NM]): Data[NM, NS] = common[NM, NS]
    }
    

    // ============ Drop attribute

    def drop[FN <: Symbol, NS <: HList](field: Witness.Aux[FN])(
        implicit
        r: RemoveField.Aux[S, FN :: HNil, NS]
    ): Data[M, NS] = new Data[M, NS] {
        type DST = Row
        val data = d.data.drop(field.value.name)
    }

    // With nesting, not really supported by the dataframe (as is).
    /* def drop[PW <: HList, PS <: HList, NS <: HList](field: Path.Aux[PW, PS])(
        implicit
        r: RemoveField.Aux[S, PS, NS]
    ): Data[M, NS] = new Data[M, NS] {
        type DST = Row
        val data = d.data.drop(field.asString)
    } */

    // ============ Update attribute

    // With new name
    def update[FN <: Symbol, NN <: Symbol, NT, F, NS <: HList](
        field: Witness.Aux[FN], new_name: Witness.Aux[NN], f: F
    )(
        implicit
        udf: FuncOnSchema.Aux[S, (FN::HNil) :: HNil, F, NT],
        r: ReplaceField.Aux[S, FN :: HNil, NN, NT, NS]
    ) = inner_update[NS](d.data.withColumnRenamed(field.value.name, new_name.value.name)
                            .withColumn(new_name.value.name, udf(f)(col(new_name.value.name))))

    // Without new name
    def update[FN <: Symbol, NT, F, NS <: HList](
        field: Witness.Aux[FN], f: F
    )(
        implicit
        udf: FuncOnSchema.Aux[S, (FN::HNil) :: HNil, F, NT],
        r: ReplaceField.Aux[S, FN :: HNil, FN, NT, NS]
    ) = inner_update[NS](d.data.withColumn(field.value.name, udf(f)(col(field.value.name))))

    private def inner_update[NS <: HList](
        new_ds: Dataset[Row]
    ) = new {
        private def common[A <: Model, B <: HList]: Data[A,B] = new Data[A,B]{
            type DST = Row
            val data = new_ds
        }

        // Without model change
        def keepModel(implicit isValid: Model.As[NS, M]): Data[M, NS] = common[M, NS]

        // With model change
        def changeModel[NM <: Model](implicit isValid: Model.As[NS, NM]): Data[NM, NS] = common[NM, NS]
    }

    // ============ Rename attribute

    def withColumnRenamed[FN <: Symbol, FT, NN <: Symbol, NS <: HList](
        old_name: Witness.Aux[FN], new_name: Witness.Aux[NN]
    )(
        implicit
        s: SelectField.Aux[S, FN :: HNil, FN, FT],
        r: ReplaceField.Aux[S, FN :: HNil, NN, FT, NS]
    ): Data[M, NS] = new Data[M, NS] {
        type DST = Row
        val data = d.data.withColumnRenamed(old_name.value.name, new_name.value.name)
    }

    // With nesting, not really supported by the dataframe (as is).
    /* def withColumnRenamed[PW1 <: HList, PS1 <: HList, FN <: Symbol, FT, NN <: Symbol, NS <: HList](
        old_name: Path.Aux[PW1, PS1], new_name: Witness.Aux[NN]
    )(
        implicit
        s: SelectField.Aux[S, PS1, FN, FT],
        r: ReplaceField.Aux[S, PS1, NN, FT, NS]
    ): Data[M, NS] = new Data[M, NS] {
        type DST = Row
        val data = d.data.withColumnRenamed(old_name.asString, new_name.value.name)
    } */

    // ============ Join between datasets

    //type JoinModes = Witness.`"inner"`.T :: Witness.`"cross"`.T :: Witness.`"outer"`.T :: Witness.`"full"`.T :: Witness.`"fullouter"`.T :: Witness.`"full_outer"`.T :: Witness.`"left"`.T :: Witness.`"leftouter"`.T :: Witness.`"left_outer"`.T :: Witness.`"right"`.T :: Witness.`"rightouter"`.T :: Witness.`"right_outer"`.T :: Witness.`"semi"`.T :: Witness.`"leftsemi"`.T :: Witness.`"left_semi"`.T :: Witness.`"anti"`.T :: Witness.`"leftanti"`.T :: Witness.`"left_anti"`.T :: HNil
    type JoinModes = Witness.`"inner"`.T :: Witness.`"cross"`.T :: Witness.`"outer"`.T :: Witness.`"full"`.T :: Witness.`"fullouter"`.T :: Witness.`"full_outer"`.T :: Witness.`"left"`.T :: Witness.`"leftouter"`.T :: Witness.`"left_outer"`.T :: Witness.`"right"`.T :: Witness.`"rightouter"`.T :: Witness.`"right_outer"`.T :: HNil

    def join[S2 <: HList, M2 <: Model, O <: FOperator, I, JM <: String, NS <: HList](
        d2: Data[M2, S2], cond: FilterOps[O,I], joinType: Witness.Aux[JM] = Witness("inner")
    )(
        implicit
        //@implicitNotFound("[Pridwen / Dataset] Unknown join mode ${JM}. Please chose one among inner, cross, outer, full, fullouter, full_outer, left, leftouter, left_outer, right, rightouter, right_outer, semi, leftsemi, left_semi, anti, leftanti, left_anti.") 
        @implicitNotFound("[Pridwen / Dataset] Unknown join mode ${JM}. Please chose one among inner, cross, outer, full, fullouter, full_outer, left, leftouter, left_outer, right, rightouter, right_outer.") 
        modeExists: Selector[JoinModes, JM],
        j: JoinOps.Aux[S, S2, O, I, NS]
    ) = new {
        private def common[A <: Model, B <: HList]: Data[A,B] = new Data[A,B]{
            type DST = Row
            val data = d.data.join(d2.data, j.toSparkColumn(d.data,d2.data), joinType.value)
        }

        // Without model change
        def keepLeftModel(implicit isValid: Model.As[NS, M]): Data[M, NS] = common[M, NS]
        def keepRightModel(implicit isValid: Model.As[NS, M2]): Data[M2, NS] = common[M2, NS]

        // With model change
        def changeModel[NM <: Model](implicit isValid: Model.As[NS, NM]): Data[NM, NS] = common[NM, NS]
    }

    // ============ Aggregate datasets
    
    def groupBy[MPW <: HList, MPS <: HList, P <: HList, F <: HList, GS <: HList, NS <: HList](
        columns: MultiplePaths.Aux[MPW, MPS]
    ) = new {
        def agg[O <: AggOperator, I](a: AggOps[O, I])(
            implicit
            op: AggOps.Compute.Aux[S, O, I, P, F],
            select_grouped: SelectMany.Aux[S, MPS, GS],
            add: AddMany.Aux[GS, P, F, NS]
        ) = new {
                private def common[A <: Model, B <: HList]: Data[A,B] = new Data[A,B]{
                    type DST = Row
                    val data = { 
                        val fagg = op.toSparkColumn
                        op.newNames.foldLeft(
                            d.data.groupBy(columns.toColumns:_*).agg(fagg.head, fagg.tail:_*)
                        )((ds, nn) => ds.withColumnRenamed(s"${nn._1}(${nn._2})", nn._1))
                    }
                }

                // Without model change
                def keepModel(implicit isValid: Model.As[NS, M]): Data[M, NS] = common[M, NS]

                // With model change
                def changeModel[NM <: Model](implicit isValid: Model.As[NS, NM]): Data[NM, NS] = common[NM, NS]
            }
    }

    // ============ Sort datasets

    def orderBy[O <: OOperator, I](f: OrderOps[O,I])(implicit columns: OrderOps.Compute[S, O, I]): Data[M, S] = new Data[M, S] {
        type DST = d.DST
        val data = d.data.orderBy(columns.toSparkColumn:_*)
    }
}