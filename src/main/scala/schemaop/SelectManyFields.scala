package pridwen.schemaop

import shapeless.{HList, ::, HNil}
import shapeless.labelled.{FieldType => Field}

trait SelectManyFields[Schema <: HList, Paths_To_Fields <: HList] { type Out <: HList ; def apply(schema: Schema): Out }
object SelectManyFields {
    type Aux[Schema <: HList, Paths_To_Fields <: HList, New_Schema <: HList] = SelectManyFields[Schema, Paths_To_Fields] { type Out = New_Schema }

    protected def inhabit_Type[Schema <: HList, Paths_To_Fields <: HList, New_Schema <: HList](
        f: Schema => New_Schema
    ): Aux[Schema, Paths_To_Fields, New_Schema] = new SelectManyFields[Schema, Paths_To_Fields] { type Out = New_Schema ; def apply(schema: Schema) = f(schema) }

    implicit def there_is_at_least_one_path [
        Schema <: HList, Path, OtherPaths <: HList, 
        FName, FType, 
        New_Schema <: HList
    ](
        implicit
        select_field: SelectField.Aux[Schema, Path, FName, FType],
        select_ofields: SelectManyFields.Aux[Schema, OtherPaths, New_Schema]
    ) = inhabit_Type[Schema, Path::OtherPaths, Field[FName, FType]::New_Schema](
        (schema: Schema) => select_field(schema) :: select_ofields(schema)
    )

    implicit def there_is_no_path[Schema <: HList] = inhabit_Type[Schema, HNil.type, HNil]((schema: Schema) => HNil) 
    implicit def there_is_no_path2[Schema <: HList] = inhabit_Type[Schema, HNil, HNil]((schema: Schema) => HNil) 
}