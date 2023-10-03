package pridwen.models.aux

import shapeless.{HList, HNil, ::, Witness, Lazy}
import shapeless.labelled.{FieldType => Field, field}

import pridwen.models.{Model}
import pridwen.support.{RSelector}
import pridwen.support.functions.{getFieldValue}

trait As[FName, Path <: HList]
object As {
    def apply[Path <: HList](field_name: Witness, path: Path) = new As[field_name.T, Path] {}
}

trait SelectField[Schema <: HList, Path] { type FName ; type FType ; def apply(schema: Schema): Field[FName, FType] }
object SelectField {
    def apply[Schema <: HList, Path](implicit ok: SelectField[Schema, Path]): Aux[Schema, Path, ok.FName, ok.FType] = ok
    type Aux[Schema <: HList, Path, FName0, FType0] = SelectField[Schema, Path] { type FName = FName0 ; type FType = FType0 }

    protected def inhabit_Type[Schema <: HList, Path, FName0, FType0](
        f: Schema => Field[FName0, FType0]
    ): Aux[Schema, Path, FName0, FType0] 
        = new SelectField[Schema, Path] { 
            type FName = FName0 ; type FType = FType0 
            def apply(schema: Schema) = f(schema) 
    }

    implicit def schema_is_nested [
        Schema <: HList, F, OtherFields <: HList, 
        FName, FSchema <: HList, 
        FName0, FType0
    ](
        implicit
        select_field: RSelector.Aux[Schema, F, FName, FSchema],
        recurse_path: SelectField.Aux[FSchema, OtherFields, FName0, FType0]
    ) = inhabit_Type[Schema, F::OtherFields, FName0, FType0](
        (schema: Schema) => recurse_path(select_field(schema))
    )

    implicit def schema_is_not_nested [
        Schema <: HList, F, 
        FName, FType
    ](
        implicit
        rs: RSelector.Aux[Schema, F, FName, FType]
    ) = inhabit_Type[Schema, F::HNil, FName, FType](
        (s: Schema) => field[FName](rs(s))
    )

    implicit def path_with_alias [
        Schema <: HList, Alias <: Symbol, Path <: HList, 
        FName0, FType0
    ](
        implicit
        select_field: Lazy[SelectField.Aux[Schema, Path, FName0, FType0]]
    ) = inhabit_Type[Schema, As[Alias, Path], Alias, FType0](
        (schema: Schema) => field[Alias](getFieldValue(select_field.value(schema)))
    )

    // Same as schema_is_not_nested but only requires the field name (rather than field_name :: HNil)
    implicit def schema_is_not_nested2 [Schema <: HList, FName <: Symbol, FName0, FType0](
        implicit
        select_field: RSelector.Aux[Schema, FName, FName0, FType0]
    ) = inhabit_Type[Schema, FName, FName0, FType0](
        (schema: Schema) => field[FName0](select_field(schema))
    )
}