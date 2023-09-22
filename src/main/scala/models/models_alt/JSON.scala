package pridwen.models.alt

import shapeless.{HList, HNil, ::}
import shapeless.labelled.{FieldType}

import java.time.{LocalDate => Date}

import pridwen.support.{DeepGeneric}

abstract class JSON[S](dataset: List[S]) extends Model[S](dataset)
object JSON { 
    type Aux[S, Repr0 <: HList] = JSON[S] { type Repr = Repr0 }
    def apply[S](dataset: List[S])(implicit ok: ValidJSON[S]) = ok(dataset)
}

trait ValidJSON[S] { type Out <: JSON[_] ; def apply(dataset: List[S]): Out }
object ValidJSON {
    def apply[S](implicit ok: ValidJSON[S]): Aux[S, ok.Out] = ok
    type Aux[S, Out0 <: JSON[_]] = ValidJSON[S] { type Out = Out0 }
    protected def inhabit_Type[S, Repr0 <: HList](f: S => Repr0): Aux[S, JSON.Aux[S, Repr0]] = new ValidJSON[S] { type Out = JSON.Aux[S, Repr0] ; def apply(dataset: List[S]) = new JSON[S](dataset) { type Repr = Repr0 ; def toRepr(s: S) = f(s) } }

    implicit def case_class_schema[CCS <: Product, S <: HList](
        implicit
        gen: DeepGeneric.Aux[CCS, S],
        j: ValidJSON[S]
    ) = inhabit_Type[CCS, S](
        (s: CCS) => gen.to(s)
    )

    implicit def empty_schema = inhabit_Type[HNil, HNil]((s: HNil) => HNil)
    implicit def schema_with_multiple_attributes[K, V, T <: HList](
        implicit
        r: JType[V],
        i: ValidJSON[T]
    ) = inhabit_Type[FieldType[K,V]::T, FieldType[K,V]::T](
        (s: FieldType[K,V]::T) => s
    )

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
        //implicit def json_hlist = inhabit_JType[HList]
        implicit def json_multi[T : JType] = inhabit_JType[List[T]]
        implicit def json_nested[H <: HList : ValidJSON] = inhabit_JType[H]
    }
}