package pridwen.models

import shapeless.{HList, HNil, ::, Lazy}
import shapeless.labelled.{FieldType}

import java.time.{LocalDate => Date}

trait JSON[S <: HList] extends Model { type Schema = S }
trait SomeJSON extends JSON[HNil]
object JSON {
    type Aux[S <: HList, Schema0 <: HList] = JSON[S] { type Schema = Schema0 }
    type IsConform[S <: HList] = Aux[S, S]
    def apply[S <: HList](d: List[S])(implicit ok: JSON[S]): Aux[S, ok.Schema] = new JSON[S] { override type Schema = ok.Schema ; val data = d }
    private def inhabit_Type[S <: HList]: Aux[S, S] = new JSON[S] { override type Schema = S ; val data = List() }

    implicit def empty_schema = inhabit_Type[HNil]
    implicit def schema_with_multiple_attributes[K, V, T <: HList](
        implicit
        r: JType[V],
        i: JSON.IsConform[T]
    ) = inhabit_Type[FieldType[K,V]::T]

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
        implicit def json_nested[H <: HList : JSON.IsConform] = inhabit_JType[H]
    }
}