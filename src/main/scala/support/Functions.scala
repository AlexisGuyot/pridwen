package pridwen.support

import shapeless.{HList, Witness}
import shapeless.labelled.{FieldType => Field, field}
import shapeless.ops.record.{Selector => RSelector}

object functions {
    def getFieldValue[FName,FType](f: Field[FName,FType]): FType = f

    def rename[FName, FType](f: Field[FName,FType], new_name: Witness): Field[new_name.T,FType] = field[new_name.T](getFieldValue(f))

    def get[H <: HList, FType](hlist: H, fname: Witness)(implicit select_field: RSelector[H, fname.T]): select_field.Out = select_field(hlist)
}