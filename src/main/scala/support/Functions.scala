package pridwen.support

import shapeless.{HList, Witness}
import shapeless.labelled.{FieldType, field}
import shapeless.ops.record.{Selector => RSelector}

import pridwen.models.aux.{SelectAtt}

object functions {
    def getFieldValue[K,V](f: FieldType[K,V]): V = f

    def rename[OK, V](f: FieldType[OK, V], name: Witness): FieldType[name.T, V] = field[name.T](getFieldValue(f))

    def get[H <: HList, V](h: H, n: Witness)(implicit get_n: RSelector[H, n.T]): get_n.Out = get_n(h)
}