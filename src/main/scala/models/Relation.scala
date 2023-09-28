package pridwen.models

import shapeless.{HList, HNil, ::, Lazy}
import shapeless.labelled.{FieldType}

import java.time.{LocalDate => Date}

import pridwen.support.{DeepGeneric}

abstract class Relation[S](dataset: List[S]) extends Model[S](dataset)
object Relation { 
    type Aux[S, Repr0 <: HList] = Relation[S] { type Repr = Repr0 }
    def apply[S](dataset: List[S])(implicit ok: ValidRelation[S]) = ok(dataset)
}

trait ValidRelation[S] { type Out <: HList ; def apply(dataset: List[S]): Relation.Aux[S, Out] }
object ValidRelation {
    def apply[S](implicit ok: ValidRelation[S]): Aux[S, ok.Out] = ok
    type Aux[S, Out0 <: HList] = ValidRelation[S] { type Out = Out0 }
    protected def inhabit_Type[S, Repr0 <: HList](f: S => Repr0): Aux[S, Repr0] = new ValidRelation[S] { type Out = Repr0 ; def apply(dataset: List[S]) = new Relation[S](dataset) { type Repr = Repr0 ; def toRepr(s: S) = f(s) } }

    implicit def case_class_schema[CCS <: Product, S <: HList](
        implicit
        gen: DeepGeneric.Aux[CCS, S],
        g: ValidRelation[S]
    ) = inhabit_Type[CCS, S](
        (s: CCS) => gen.to(s)
    )

    implicit def empty_schema = inhabit_Type[HNil, HNil]((s: HNil) => HNil)
    implicit def schema_with_multiple_attributes[K, V, T <: HList](
        implicit
        r: RType[V],
        i: ValidRelation[T]
    ) = inhabit_Type[FieldType[K,V]::T, FieldType[K,V]::T](
        (s: FieldType[K,V]::T) => s
    )

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