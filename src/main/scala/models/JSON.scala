package pridwen.models

import shapeless.{HList, HNil, ::}
import shapeless.labelled.{FieldType => Field}

import java.time.{LocalDate => Date}

import pridwen.support.{DeepGeneric}



// ========================= Type definition

abstract class JSON[Schema](dataset: List[Schema]) extends Model[Schema](dataset)
object JSON { 
    type Aux[Schema, Repr0 <: HList] = JSON[Schema] { type Repr = Repr0 }
    def apply[Schema](dataset: List[Schema])(implicit ok: IsValidJSON[Schema]) = ok(dataset)
}



// ========================= To verify that a schema conforms to the JSON model

trait IsValidJSON[Schema] { type Out <: HList ; def apply(dataset: List[Schema]): JSON.Aux[Schema, Out] }
object IsValidJSON {
    def apply[Schema](implicit ok: IsValidJSON[Schema]): Aux[Schema, ok.Out] = ok
    type Aux[Schema, Out0 <: HList] = IsValidJSON[Schema] { type Out = Out0 }

    protected def inhabit_Type[Schema, Repr0 <: HList](
        f: Schema => Repr0
    ): Aux[Schema, Repr0] 
        = new IsValidJSON[Schema] { 
            type Out = Repr0 
            def apply(dataset: List[Schema]) = new JSON[Schema](dataset) { type Repr = Repr0 ; def toRepr(schema: Schema) = f(schema) } 
    }

    implicit def schema_as_case_class[CSchema <: Product, HSchema <: HList](
        implicit
        convert: DeepGeneric.Aux[CSchema, HSchema],
        json: IsValidJSON[HSchema]
    ) = inhabit_Type[CSchema, HSchema](
        (schema: CSchema) => convert.to(schema)
    )

    implicit def schema_has_at_least_one_field[FName, FType, OtherFields <: HList](
        implicit
        valid_value_type: JType[FType],
        json: IsValidJSON[OtherFields]
    ) = inhabit_Type[Field[FName,FType]::OtherFields, Field[FName,FType]::OtherFields](
        (schema: Field[FName,FType]::OtherFields) => schema
    )

    implicit def schema_is_empty = inhabit_Type[HNil, HNil]((schema: HNil) => HNil)

    // ========================= Valid value types for this model

    private trait JType[T]
    private object JType {
        def apply[T](implicit ok: JType[T]): JType[T] = ok
        private def inhabit_JType[T]: JType[T] = new JType[T] {}

        implicit def json_string = inhabit_JType[String]
        implicit def json_int = inhabit_JType[Int]
        implicit def json_double = inhabit_JType[Double]
        implicit def json_long = inhabit_JType[Long]
        implicit def json_bool = inhabit_JType[Boolean]
        implicit def json_date = inhabit_JType[Date]
        implicit def json_multi[T : JType] = inhabit_JType[List[T]] // NB: homogeneous collection
        implicit def json_nested[Schema <: HList : IsValidJSON] = inhabit_JType[Schema]
    }
}