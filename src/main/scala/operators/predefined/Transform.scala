package pridwen.operators.predefined

import shapeless.{HList, HNil, ::, Witness}
import shapeless.labelled.{FieldType => Field, field}
import shapeless.ops.hlist.{Prepend}

import pridwen.models.aux.{ValidModel, AddAtt, SelectAtt, UpdateSchema}
import pridwen.models.{Model, Graph, ValidGraph}
import pridwen.support.{EquivHList}

object transform {
    def transform [
        ModelIn <: Model[_], Transformations <: HList, New_Schema <: HList
    ](
        dataset: ModelIn, 
        transformations: Transformations
    )(
        f: dataset.type => List[New_Schema]
    )(
        implicit
        update_schema: UpdateSchema.Aux[dataset.Repr, Transformations, New_Schema],
        res_model: ValidModel[dataset.type, New_Schema, HNil]
    ): res_model.Out = res_model(f(dataset))

    def transform [
        Schema, SourceID, DestID, Transformations <: HList, New_Schema <: HList
    ](
        dataset: Graph[Schema, SourceID, DestID], 
        transformations: Transformations
    )(
        f: dataset.type => List[New_Schema]
    )(
        implicit
        update_schema: UpdateSchema.Aux[dataset.Repr, Transformations, New_Schema],
        res_model: ValidGraph[New_Schema, SourceID, DestID]
    ): Graph.Aux[New_Schema, SourceID, DestID, res_model.E, res_model.V] = res_model(f(dataset))
}