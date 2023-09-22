package pridwen.models.alt

import shapeless.{HList}

abstract class Model[S](dataset: List[S]) { type Repr <: HList ; def toRepr(s: S): Repr ; val data: List[Repr] = dataset.map(s => toRepr(s)) }
