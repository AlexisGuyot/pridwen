package pridwen.models

import shapeless.{HList, HNil, ::, Lazy}
import shapeless.labelled.{FieldType}

import java.time.{LocalDate => Date}

import pridwen.support.{DeepGeneric}

trait Relation[S] extends Model[S]
object Relation {
    type Aux[S, Repr0 <: HList] = Relation[S] { type Repr = Repr0 }
    def apply[S](implicit ok: Relation[S]): Aux[S, ok.Repr] = ok
    private def inhabit_Type[S, Repr0 <: HList](f: S => Repr0): Aux[S, Repr0] = new Relation[S] { type Repr = Repr0 ; def toRepr(s: S) = f(s) }
    
    def load[S](r: Relation[S])(dataset: List[S]) = new Relation[S] { type Repr = r.Repr ; override val data = dataset.map(s => toRepr(s)) ; def toRepr(s: S) = r.toRepr(s) }

    implicit def case_class_schema[CCS <: Product, S <: HList](
        implicit
        gen: DeepGeneric.Aux[CCS, S],
        g: Relation[S]
    ) = inhabit_Type[CCS, S](
        (s: CCS) => gen.to(s)
    )

    implicit def empty_schema = inhabit_Type[HNil, HNil]((s: HNil) => HNil)
    implicit def schema_with_multiple_attributes[K, V, T <: HList](
        implicit
        r: RType[V],
        i: Relation[T]
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