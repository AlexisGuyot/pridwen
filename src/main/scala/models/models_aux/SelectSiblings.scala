package pridwen.models.aux

import shapeless.{HList, HNil, ::}

import pridwen.support.{ReducePath}



trait SelectSiblings[Schema <: HList, Path <: HList] { type Out <: HList ; def apply(schema: Schema): Out }
object SelectSiblings {
    def apply[Schema <: HList, Path <: HList](implicit ok: SelectSiblings[Schema, Path]): Aux[Schema, Path, ok.Out] = ok
    type Aux[Schema <: HList, Path <: HList, New_Schema <: HList] = SelectSiblings[Schema, Path] { type Out = New_Schema }

    protected def inhabit_Type[Schema <: HList, Path <: HList, New_Schema <: HList](
        f: Schema => New_Schema
    ): Aux[Schema, Path, New_Schema] 
        = new SelectSiblings[Schema, Path] { 
            type Out = New_Schema 
            def apply(schema: Schema) = f(schema) 
    }

    implicit def schema_is_nested [
        Schema <: HList, Path <: HList, 
        ReducedPath <: HList, 
        ParentName, ParentSchema <: HList]
    (
        implicit
        reduce_path: ReducePath.Aux[Path, ReducedPath],
        select_field: SelectField.Aux[Schema, ReducedPath, ParentName, ParentSchema]
    ) = inhabit_Type[Schema, Path, ParentSchema](
        (schema: Schema) => select_field(schema)
    )

    implicit def schema_is_not_nested [
        Schema <: HList, F
    ](
        implicit
        select_field: SelectField[Schema, F::HNil]
    ) = inhabit_Type[Schema, F::HNil, Schema](
        (schema: Schema) => schema
    )
}