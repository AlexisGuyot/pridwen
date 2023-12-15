package pridwen.models

import shapeless.{HList, HNil, ::}
import shapeless.labelled.{FieldType => Field}

import java.time.{LocalDate => Date}

import pridwen.support.{DeepGeneric}



// ========================= Type definition

abstract class Relation[Schema](dataset: List[Schema]) extends Model[Schema](dataset)
object Relation { 
    type Aux[Schema, Repr0 <: HList] = Relation[Schema] { type Repr = Repr0 }
    def apply[Schema](dataset: List[Schema])(implicit ok: IsValidRelation[Schema]) = ok(dataset)
}


// ========================= To verify that a schema conforms to the relational model

trait IsValidRelation[Schema] { type Out <: HList ; def apply(dataset: List[Schema]): Relation.Aux[Schema, Out] }
object IsValidRelation {
    def apply[Schema](implicit ok: IsValidRelation[Schema]): Aux[Schema, ok.Out] = ok
    type Aux[Schema, Out0 <: HList] = IsValidRelation[Schema] { type Out = Out0 }

    protected def inhabit_Type[Schema, Repr0 <: HList](
        f: Schema => Repr0
    ): Aux[Schema, Repr0] 
        = new IsValidRelation[Schema] { 
            type Out = Repr0 
            def apply(dataset: List[Schema]) = new Relation[Schema](dataset) { type Repr = Repr0 ; def toRepr(schema: Schema) = f(schema) } 
    }

    implicit def schema_as_case_class[CSchema <: Product, HSchema <: HList](
        implicit
        convert: DeepGeneric.Aux[CSchema, HSchema],
        relation: IsValidRelation[HSchema]
    ) = inhabit_Type[CSchema, HSchema](
        (schema: CSchema) => convert.to(schema)
    )

    implicit def schema_has_at_least_one_field[FName, FType, OtherFields <: HList](
        implicit
        valid_value_type: RType[FType],
        relation: IsValidRelation[OtherFields]
    ) = inhabit_Type[Field[FName,FType]::OtherFields, Field[FName,FType]::OtherFields](
        (schema: Field[FName,FType]::OtherFields) => schema
    )

    implicit def schema_is_empty = inhabit_Type[HNil, HNil]((s: HNil) => HNil)

    // ========================= Valid value types for this model

    private trait RType[T]
    private object RType {
        def apply[T](implicit ok: RType[T]): RType[T] = ok
        private def inhabit_RType[T]: RType[T] = new RType[T] {}

        implicit def rel_string = inhabit_RType[String]
        implicit def rel_int = inhabit_RType[Int]
        implicit def rel_double = inhabit_RType[Double]
        implicit def rel_long = inhabit_RType[Long]
        implicit def rel_bool = inhabit_RType[Boolean]
        implicit def rel_date = inhabit_RType[Date]
    }
}