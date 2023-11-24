package pridwen.operators

import shapeless.{HList, HNil}

import pridwen.models._
import pridwen.models.aux.{TransformSchema}

object transform {
    def transform[ModelIn <: Model, Transfo <: HList, New_Schema <: HList, Out](
        dataset: ModelIn,
        transformations: Transfo
    )(
        f: dataset.type => List[New_Schema]
    )(
        implicit
        update_schema: TransformSchema.Aux[dataset.Schema, Transfo, New_Schema]
    ) = new {
        def apply(implicit new_dataset: Model.As.Aux[New_Schema, ModelIn, Out]): Out = new_dataset(f(dataset))
        def to[ModelOut <: Model](implicit new_dataset: Model.As.Aux[New_Schema, ModelOut, Out]): Out = new_dataset(f(dataset))
    }

    def transform[ModelIn <: Model, New_Schema <: HList, Out](
        dataset: ModelIn
    )(
        f: dataset.type => List[New_Schema]
    ) = new {
        def apply(implicit new_dataset: Model.As.Aux[New_Schema, ModelIn, Out]): Out = new_dataset(f(dataset))
        def to[ModelOut <: Model](implicit new_dataset: Model.As.Aux[New_Schema, ModelOut, Out]): Out = new_dataset(f(dataset))
    }
}