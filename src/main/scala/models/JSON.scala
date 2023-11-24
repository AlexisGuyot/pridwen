package pridwen.models

import shapeless.{HList, ::, HNil}
import shapeless.labelled.{FieldType => Field}

import java.time.{LocalDate => Date}

import pridwen.support.DeepGeneric

trait JSON extends Model
object JSON {
    type Aux[Schema0 <: HList] = JSON { type Schema = Schema0 }
    
    def apply[Schema](dataset: List[Schema])(implicit isValid: ValidSchema[Schema]): isValid.T = isValid(dataset)

    trait ValidSchema[S] { type T ; def apply(dataset: List[S]): T }
    object ValidSchema {
        type Aux[S, T0] = ValidSchema[S] { type T = T0 }

        private def inhabit_Type[S, Repr0 <: HList](
            f: S => Repr0
        ): Aux[S, JSON.Aux[Repr0]] = new ValidSchema[S] {
            type T = JSON.Aux[Repr0]
            def apply(dataset: List[S]): T = new JSON {
                type Schema = Repr0
                val data = convert_to_repr(dataset, f)
            }
        }

        implicit def schema_as_case_class[CSchema <: Product, HSchema <: HList](
            implicit
            convert: DeepGeneric.Aux[CSchema, HSchema],
            json: ValidSchema[HSchema]
        ) = inhabit_Type[CSchema, HSchema](
            (schema: CSchema) => convert.to(schema)
        )

        implicit def schema_has_at_least_one_field[FName, FType, OtherFields <: HList](
            implicit
            valid_value_type: JType[FType],
            json: ValidSchema[OtherFields]
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
            implicit def json_nested[Schema <: HList : ValidSchema] = inhabit_JType[Schema]
        }
    }
}