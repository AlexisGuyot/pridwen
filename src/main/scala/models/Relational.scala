/* package pridwen.models

import shapeless.{HList, HNil, ::, Lazy}
import shapeless.labelled.{FieldType}

import java.time.{LocalDate => Date}

trait Relation[S <: HList] extends Model { type Schema = S }
trait SomeRelation extends Relation[HNil]
object Relation extends Relation[HNil] {
    type Aux[S <: HList, Schema0 <: HList] = Relation[S] { type Schema = Schema0 }
    type IsConform[S <: HList] = Aux[S, S]
    def apply[S <: HList](implicit ok: Relation[S]): Aux[S, ok.Schema] = ok
    private def inhabit_Type[S <: HList]: Aux[S, S] = new Relation[S] { override type Schema = S }

    implicit def empty_schema = inhabit_Type[HNil]
    implicit def schema_with_multiple_attributes[K, V, T <: HList](
        implicit
        r: RType[V],
        i: Relation.IsConform[T]
    ) = inhabit_Type[FieldType[K,V]::T]/*  */

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
} */