package pridwen.schemaop

import shapeless.{HList, HNil, ::, Witness}
import shapeless.labelled.{FieldType => Field}

import scala.reflect.runtime.universe.TypeTag

trait PrintSchema[Schema <: HList] { def apply(prefix: String = ""): String }
trait LowPriorityPrintSchema {
    protected def inhabit_Type[Schema <: HList](
        f: String => String
    ): PrintSchema[Schema] = new PrintSchema[Schema] { def apply(prefix: String = "") = f(prefix) }

    implicit def head_is_not_nested[FName <: Symbol, FType, OtherFields <: HList](
        implicit
        get_fname: Witness.Aux[FName],
        get_ftype: TypeTag[FType],
        print_ofields: PrintSchema[OtherFields]
    ) = inhabit_Type[Field[FName, FType]::OtherFields]((prefix: String) =>  "\n" + prefix + "- " + get_fname.value.name + ": " + get_ftype.tpe + print_ofields(prefix))
}
object PrintSchema extends LowPriorityPrintSchema {
    implicit def head_is_nested[FName <: Symbol, FSchema <: HList, OtherFields <: HList](
        implicit
        get_fname: Witness.Aux[FName],
        print_fschema: PrintSchema[FSchema],
        print_ofields: PrintSchema[OtherFields]
    ) = inhabit_Type[Field[FName, FSchema]::OtherFields]((prefix: String) => prefix + "- " + get_fname.value.name + ":" + print_fschema("\t" + prefix) + "\n" + print_ofields(prefix))

    implicit def empty_schema = inhabit_Type[HNil]((prefix: String) => "")
}