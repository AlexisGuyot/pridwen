package pridwen.models

import shapeless.{HList, ::, HNil}
import shapeless.labelled.{FieldType => Field}

import java.time.{LocalDate => Date}

import pridwen.support.DeepGeneric

trait Relation extends Model
object Relation {
    type Aux[Schema0 <: HList] = Relation { type Schema = Schema0 }
    def apply[Schema](dataset: List[Schema])(implicit isValid: ValidSchema[Schema]): isValid.T = isValid(dataset)

    trait ValidSchema[S] { type T ; def apply(dataset: List[S]): T }
    object ValidSchema {
        type Aux[S, T0] = ValidSchema[S] { type T = T0 }

        private def inhabit_Type[S, Repr0 <: HList](
            f: S => Repr0
        ): Aux[S, Relation.Aux[Repr0]] = new ValidSchema[S] {
            type T = Relation.Aux[Repr0]
            def apply(dataset: List[S]): Relation.Aux[Repr0] = new Relation {
                type Schema = Repr0
                val data = convert_to_repr(dataset, f)
            }
        }

        implicit def schema_as_case_class[CSchema <: Product, HSchema <: HList](
            implicit
            convert: DeepGeneric.Aux[CSchema, HSchema],
            relation: ValidSchema[HSchema]
        ) = inhabit_Type[CSchema, HSchema](
            (schema: CSchema) => convert.to(schema)
        )

        implicit def schema_has_at_least_one_field[FName, FType, OtherFields <: HList](
            implicit
            valid_value_type: RType[FType],
            relation: ValidSchema[OtherFields]
        ) = inhabit_Type[Field[FName,FType]::OtherFields, Field[FName,FType]::OtherFields](
            (schema: Field[FName,FType]::OtherFields) => schema
        )

        implicit def schema_is_empty = inhabit_Type[HNil, HNil]((s: HNil) => HNil)

        // ========================= Valid value types for this model

        private trait RType[T]
        private object RType {
            private def inhabit_RType[T]: RType[T] = new RType[T] {}

            implicit def rel_string = inhabit_RType[String]
            implicit def rel_int = inhabit_RType[Int]
            implicit def rel_double = inhabit_RType[Double]
            implicit def rel_long = inhabit_RType[Long]
            implicit def rel_bool = inhabit_RType[Boolean]
            implicit def rel_date = inhabit_RType[Date]
        }
    }
}