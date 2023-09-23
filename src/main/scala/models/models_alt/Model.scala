package pridwen.models.alt

import shapeless.{HList, HNil, ::, Witness => W}
import shapeless.labelled.{FieldType => Field, field}

abstract class Model[S](dataset: List[S]) { type Repr <: HList ; def toRepr(s: S): Repr ; val data: List[Repr] = dataset.map(s => toRepr(s)) }
object Model {
    def JSON(implicit j: ValidJSON[HNil]) = j(List(HNil))
    def Relation(implicit r: ValidRelation[HNil]) = r(List(HNil))
    def Graph(implicit g: ValidGraph[(Field[W.`'id`.T, Int] :: HNil) :: (Field[W.`'id`.T, Int] :: HNil) :: HNil, W.`'id`.T, W.`'id`.T]) = g(List((field[W.`'id`.T](0)::HNil) :: (field[W.`'id`.T](0)::HNil) :: HNil))
}