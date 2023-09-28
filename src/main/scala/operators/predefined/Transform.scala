package pridwen.operators.predefined

import shapeless.{HList, HNil, ::, Witness}
import shapeless.labelled.{FieldType => Field, field}
import shapeless.ops.hlist.{Prepend}

import pridwen.models.aux.{ValidModel, AddAtt, SelectAtt, UpdateSchema}
import pridwen.models.{Model, Graph, ValidGraph}
import pridwen.support.{EquivHList}

object transform {
    def transform[MI <: Model[_], T <: HList, S <: HList, New_Schema <: HList, Out](dataset: MI, transformations: T)(f: dataset.type => List[New_Schema])(
        implicit
        update_schema: UpdateSchema.Aux[dataset.Repr, T, New_Schema],
        res_model: ValidModel[dataset.type, New_Schema, HNil]
    ): res_model.Out = res_model(f(dataset))

    def transform[MI <: Model[_], T <: HList, S, SID, DID, New_Schema <: HList, Out](dataset: Graph[S, SID, DID], transformations: T)(f: dataset.type => List[New_Schema])(
        implicit
        update_schema: UpdateSchema.Aux[dataset.Repr, T, New_Schema],
        res_model: ValidGraph[New_Schema, SID, DID]
    ): Graph.Aux[New_Schema, SID, DID, res_model.E, res_model.V] = res_model(f(dataset))
}