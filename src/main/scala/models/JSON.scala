package pridwen.models

import shapeless.{HList, HNil, ::, Lazy}
import shapeless.labelled.{FieldType}

import java.time.{LocalDate => Date}

import pridwen.support.{DeepGeneric}

trait JSON[S] extends Model[S]
object JSON {
    type Aux[S, Repr0 <: HList] = JSON[S] { type Repr = Repr0 }
    def apply[S](implicit ok: JSON[S]): Aux[S, ok.Repr] = ok
    private def inhabit_Type[S, Repr0 <: HList](f: S => Repr0): Aux[S, Repr0] = new JSON[S] { type Repr = Repr0 ; def toRepr(s: S) = f(s) }
    
    //def load[S](j: JSON[S])(dataset: List[j.Repr]) = new JSON[j.Repr] { type Repr = j.Repr ; override val data = dataset ; def toRepr(s: j.Repr) = s }
    def load[S](j: JSON[S])(dataset: List[S]) = new JSON[S] { type Repr = j.Repr ; override val data = dataset.map(s => toRepr(s)) ; def toRepr(s: S) = j.toRepr(s) }

    implicit def case_class_schema[CCS <: Product, S <: HList](
        implicit
        gen: DeepGeneric.Aux[CCS, S],
        j: JSON[S]
    ) = inhabit_Type[CCS, S](
        (s: CCS) => gen.to(s)
    )

    implicit def empty_schema = inhabit_Type[HNil, HNil]((s: HNil) => HNil)
    implicit def schema_with_multiple_attributes[K, V, T <: HList](
        implicit
        r: JType[V],
        i: JSON[T]
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
        implicit def json_nested[H <: HList : JSON] = inhabit_JType[H]
    }
}