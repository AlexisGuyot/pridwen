package pridwen.models.aux

import shapeless.{HList, HNil, ::, Witness}
import shapeless.labelled.{FieldType => Field}

import scala.reflect.runtime.universe.{TypeTag}

trait PrintSchema[S <: HList] { def apply(prefix: String = ""): String }
trait LowPriorityPrintSchema {
    protected def inhabit_Type[S <: HList](f: String => String): PrintSchema[S] = new PrintSchema[S] { def apply(prefix: String = "") = f(prefix) }

    implicit def head_is_not_nested[K <: Symbol, H, T <: HList](
        implicit
        w: Witness.Aux[K],
        tag: TypeTag[H],
        print_tail: PrintSchema[T]
    ) = inhabit_Type[Field[K, H]::T]((prefix: String) =>  "\n" + prefix + "- " + w.value.name + ": " + tag.tpe + print_tail(prefix))
}
object PrintSchema extends LowPriorityPrintSchema {
    def apply[S <: HList](implicit ok: PrintSchema[S]): PrintSchema[S] = ok

    implicit def head_is_nested[K <: Symbol, H <: HList, T <: HList](
        implicit
        w: Witness.Aux[K],
        print_head: PrintSchema[H],
        print_tail: PrintSchema[T]
    ) = inhabit_Type[Field[K, H]::T]((prefix: String) => prefix + "- " + w.value.name + ":" + print_head("\t" + prefix) + "\n" + print_tail(prefix))

    implicit def empty_schema = inhabit_Type[HNil]((prefix: String) => "")
}