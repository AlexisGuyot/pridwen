package pridwen.support

import shapeless.{Witness}
import shapeless.labelled.{FieldType, field}

import pridwen.models.aux.{SelectAtt}

object functions {
    def getFieldValue[K,V](f: FieldType[K,V]): V = f

    def rename[OK, V](f: FieldType[OK, V], name: Witness): FieldType[name.T, V] = field[name.T](getFieldValue(f))
}