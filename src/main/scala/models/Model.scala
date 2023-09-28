package pridwen.models

import shapeless.{HList, HNil, ::, Witness => W}
import shapeless.labelled.{FieldType => Field, field}

import pridwen.models.aux.{SelectAtt}

abstract class Model[S](dataset: List[S]) { 
    type T
    type Repr <: HList ; 
    def toRepr(s: S): Repr ; 
    val data: List[Repr] = dataset.map(s => toRepr(s))

    def get[P, K, V](path: P, filter: Repr => Boolean = (_ => true))(implicit select: SelectAtt.Aux[Repr, P, K, V]): List[Field[K,V]] = { val d = scala.collection.mutable.ListBuffer.empty[Field[K,V]] ; data.foreach(repr => if(filter(repr)) select(repr) +=: d) ; d.to(List) }
}
object Model {
    def JSON(implicit j: ValidJSON[HNil]) = j(List(HNil))
    def Relation(implicit r: ValidRelation[HNil]) = r(List(HNil))
    def Graph(implicit g: ValidGraph[(Field[W.`'id`.T, Int] :: HNil) :: (Field[W.`'id`.T, Int] :: HNil) :: HNil, W.`'id`.T, W.`'id`.T]) = g(List((field[W.`'id`.T](0)::HNil) :: (field[W.`'id`.T](0)::HNil) :: HNil))
}