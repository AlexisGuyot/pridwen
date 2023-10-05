package pridwen.operators

import shapeless.{HList, HNil}

import pridwen.models.aux.{IsValidSchema, UpdateSchema}
import pridwen.models.{Model, Graph, IsValidGraph}

object transform {
    //==================== With explicit transformations

    //------------ No model change

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
        res_model: IsValidSchema[New_Schema, dataset.type, HNil]
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
        res_model: IsValidGraph[New_Schema, SourceID, DestID]
    ): Graph.Aux[New_Schema, SourceID, DestID, res_model.Repr] = res_model(f(dataset))

    //------------ With model change

    def transform [
        ModelIn <: Model[_], Transformations <: HList, New_Schema <: HList, ModelOut <: Model[_]
    ](
        dataset: ModelIn, 
        transformations: Transformations,
        model_out: ModelOut
    )(
        f: dataset.type => List[New_Schema]
    )(
        implicit
        update_schema: UpdateSchema.Aux[dataset.Repr, Transformations, New_Schema],
        res_model: IsValidSchema[New_Schema, ModelOut, HNil]
    ): res_model.Out = res_model(f(dataset))

    def transform [
        Schema, SourceID, DestID, Transformations <: HList, New_Schema <: HList, ModelOut <: Model[_]
    ](
        dataset: Graph[Schema, SourceID, DestID], 
        transformations: Transformations,
        model_out: ModelOut
    )(
        f: dataset.type => List[New_Schema]
    )(
        implicit
        update_schema: UpdateSchema.Aux[dataset.Repr, Transformations, New_Schema],
        res_model: IsValidSchema[New_Schema, ModelOut, HNil]
    ): res_model.Out = res_model(f(dataset))

    def transform [
        ModelIn <: Model[_], Transformations <: HList, New_Schema <: HList, Schema, SourceID, DestID
    ](
        dataset: ModelIn, 
        transformations: Transformations,
        model_out: Graph[Schema, SourceID, DestID]
    )(
        f: dataset.type => List[New_Schema]
    )(
        implicit
        update_schema: UpdateSchema.Aux[dataset.Repr, Transformations, New_Schema],
        res_model: IsValidGraph[New_Schema, SourceID, DestID]
    ): Graph.Aux[New_Schema, SourceID, DestID, res_model.Repr] = res_model(f(dataset))



    //==================== Without explicit transformations

    //------------ No model change

    def transform [
        ModelIn <: Model[_], New_Schema <: HList
    ](
        dataset: ModelIn
    )(
        f: dataset.type => List[New_Schema]
    )(
        implicit
        res_model: IsValidSchema[New_Schema, dataset.type, HNil]
    ): res_model.Out = res_model(f(dataset))

    def transform [
        Schema, SourceID, DestID, New_Schema <: HList
    ](
        dataset: Graph[Schema, SourceID, DestID] 
    )(
        f: dataset.type => List[New_Schema]
    )(
        implicit
        res_model: IsValidGraph[New_Schema, SourceID, DestID]
    ): Graph.Aux[New_Schema, SourceID, DestID, res_model.Repr] = res_model(f(dataset))

    //------------ With model change

    def transform [
        ModelIn <: Model[_], New_Schema <: HList, ModelOut <: Model[_]
    ](
        dataset: ModelIn, 
        model_out: ModelOut
    )(
        f: dataset.type => List[New_Schema]
    )(
        implicit
        res_model: IsValidSchema[New_Schema, ModelOut, HNil]
    ): res_model.Out = res_model(f(dataset))

    def transform [
        Schema, SourceID, DestID, New_Schema <: HList, ModelOut <: Model[_]
    ](
        dataset: Graph[Schema, SourceID, DestID], 
        model_out: ModelOut
    )(
        f: dataset.type => List[New_Schema]
    )(
        implicit
        res_model: IsValidSchema[New_Schema, ModelOut, HNil]
    ): res_model.Out = res_model(f(dataset))

    def transform [
        ModelIn <: Model[_], New_Schema <: HList, Schema, SourceID, DestID
    ](
        dataset: ModelIn, 
        model_out: Graph[Schema, SourceID, DestID]
    )(
        f: dataset.type => List[New_Schema]
    )(
        implicit
        res_model: IsValidGraph[New_Schema, SourceID, DestID]
    ): Graph.Aux[New_Schema, SourceID, DestID, res_model.Repr] = res_model(f(dataset))
}