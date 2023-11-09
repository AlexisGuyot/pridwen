package pridwen.models.aux

import shapeless.{HList, HNil, ::}

import pridwen.models.{Model}

import scala.collection.mutable.{Map}

trait Index[Repr <: HList, Path_To_Key] { type Out <: Map[_, List[Repr]] ; def apply(dataset: List[Repr]): Out }
object Index {
    type Aux[Repr <: HList, Path_To_Key, Out0 <: Map[_, List[Repr]]] = Index[Repr, Path_To_Key] { type Out = Out0 }
    private def inhabit_Type[Repr <: HList, Path_To_Key, Out0 <: Map[_, List[Repr]]](f: List[Repr] => Out0): Aux[Repr, Path_To_Key, Out0] = new Index[Repr, Path_To_Key] {
        type Out = Out0
        def apply(dataset: List[Repr]) = f(dataset)
    }

    implicit def rule[Repr <: HList, Path_To_Key, KName, KType](
        implicit
        select_key: SelectField.Aux[Repr, Path_To_Key, KName, KType]
    ) = inhabit_Type[Repr, Path_To_Key, Map[KType, List[Repr]]](
        (data: List[Repr]) => {
            var index: Map[KType, List[Repr]] = Map()
            data.foreach(hlist => { val key = select_key(hlist) ; index(key) = hlist :: index.getOrElse(key, List()) })
            index
        }
    )
}