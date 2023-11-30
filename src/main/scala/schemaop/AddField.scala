package pridwen.schemaop

import shapeless.{HList, ::, HNil}
import shapeless.labelled.{FieldType => Field, field}
import shapeless.ops.hlist.{Replacer, Prepend}

import pridwen.support.{RSelector}

trait AddField[Schema <: HList, Path <: HList, Field_Name, Field_Type] { type Out <: HList ; def apply(schema: Schema, value: Field_Type): Out }
object AddField {
    type Aux[Schema <: HList, Path <: HList, Field_Name, Field_Type, New_Schema <: HList] = AddField[Schema, Path, Field_Name, Field_Type] { type Out = New_Schema }

    protected def inhabit_Type[Schema <: HList, Path <: HList, Field_Name, Field_Type, New_Schema <: HList](
        f: (Schema, Field_Type) => New_Schema
    ): Aux[Schema, Path, Field_Name, Field_Type, New_Schema] = new AddField[Schema, Path, Field_Name, Field_Type] { 
        type Out = New_Schema 
        def apply(s: Schema, a: Field_Type) = f(s, a) 
    }

    implicit def schema_is_nested [
        Schema <: HList, F, OtherFields <: HList, Field_Name, Field_Type, 
        FName, FSchema <: HList, 
        New_FSchema <: HList, New_Schema <: HList
    ](
        implicit
        select_field: RSelector.Aux[Schema, F, FName, FSchema],
        recurse_path: AddField.Aux[FSchema, OtherFields, Field_Name, Field_Type, New_FSchema],
        update_schema: Replacer.Aux[Schema, Field[FName, FSchema], Field[FName, New_FSchema], (Field[FName, FSchema], New_Schema)]
    ) = inhabit_Type[Schema, F::OtherFields, Field_Name, Field_Type, New_Schema](
        (schema: Schema, value: Field_Type) => update_schema(schema, field[FName](recurse_path(select_field(schema), value)))._2
    )

    implicit def schema_is_not_nested [
        Schema <: HList, Field_Name, Field_Type, 
        New_Schema <: HList
    ](
        implicit
        concat: Prepend.Aux[Schema, Field[Field_Name, Field_Type]::HNil, New_Schema]
    ) = inhabit_Type[Schema, HNil, Field_Name, Field_Type, New_Schema](
        (schema: Schema, value: Field_Type) => concat(schema, field[Field_Name](value)::HNil)
    )
}