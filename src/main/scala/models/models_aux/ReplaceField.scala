package pridwen.models.aux

import shapeless.{HList, HNil, ::, Lazy}
import shapeless.ops.hlist.{Replacer}
import shapeless.labelled.{FieldType => Field, field}

import pridwen.support.{RSelector}

trait ReplaceField[Schema <: HList, Pathath <: HList, New_Name, New_Type] { type Out <: HList ; def apply(schema: Schema, value: New_Type): Out }
trait LowPathriorityReplaceField {
    type Aux[Schema <: HList, Pathath <: HList, New_Name, New_Type, New_Schema <: HList] = ReplaceField[Schema, Pathath, New_Name, New_Type] { type Out = New_Schema }

    protected def inhabit_Type[Schema <: HList, Path <: HList, New_Name, New_Type, New_Schema <: HList](
        f: (Schema, New_Type) => New_Schema
    ): Aux[Schema, Path, New_Name, New_Type, New_Schema] 
        = new ReplaceField[Schema, Path, New_Name, New_Type] { 
            type Out = New_Schema 
            def apply(schema: Schema, value: New_Type) = f(schema, value) 
    }

    implicit def schema_is_nested [
        Schema <: HList, F, OtherFields <: HList, New_Name, New_Type, 
        FName, FSchema <: HList, 
        New_FSchema <: HList, New_Schema <: HList
    ](
        implicit
        select_field: RSelector.Aux[Schema, F, FName, FSchema],
        recurse_path: ReplaceField.Aux[FSchema, OtherFields, New_Name, New_Type, New_FSchema],
        update_schema: Replacer.Aux[Schema, Field[FName, FSchema], Field[FName, New_FSchema], (Field[FName, FSchema], New_Schema)]
    ) = inhabit_Type[Schema, F::OtherFields, New_Name, New_Type, New_Schema](
        (schema: Schema, value: New_Type) => update_schema(schema, field[FName](recurse_path(select_field(schema), value)))._2
    )

    implicit def schema_is_not_nested [
        Schema <: HList, F, FName, FType, New_Name, New_Type, 
        New_Schema <: HList
    ](
        implicit
        select_field: RSelector.Aux[Schema, F, FName, FType],
        update_schema: Replacer.Aux[Schema, Field[FName,FType], Field[New_Name,New_Type], (Field[FName,FType], New_Schema)]
    ) = inhabit_Type[Schema, F::HNil, New_Name, New_Type, New_Schema](
        (schema: Schema, value: New_Type) => update_schema(schema, field[New_Name](value))._2
    )
}
object ReplaceField extends LowPathriorityReplaceField {
    def apply[Schema <: HList, Path <: HList, New_Name, New_Type](implicit ok: ReplaceField[Schema, Path, New_Name, New_Type]): Aux[Schema, Path, New_Name, New_Type, ok.Out] = ok

    implicit def no_new_name [
        Schema <: HList, Path <: HList, New_Type,
        FName, FType, New_Schema <: HList
    ](
        implicit
        select_field: SelectField.Aux[Schema, Path, FName, FType],
        replace_att: Lazy[ReplaceField.Aux[Schema, Path, FName, New_Type, New_Schema]]
    ) = inhabit_Type[Schema, Path, HNil, New_Type, New_Schema](
        (schema: Schema, value: New_Type) => replace_att.value(schema, value)
    )
}