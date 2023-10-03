package pridwen.models.aux

import shapeless.{HList}

import pridwen.models._

trait IsValidSchema[Schema, TargetModel <: Model[_], -Parameters] { type Out ; def apply(dataset: List[Schema]): Out }
object IsValidSchema {
    def apply[Schema, TargetModel <: Model[_], Parameters](implicit ok: IsValidSchema[Schema, TargetModel, Parameters]): Aux[Schema, TargetModel, Parameters, ok.Out] = ok
    type Aux[Schema, TargetModel <: Model[_], Parameters, ResModel] = IsValidSchema[Schema, TargetModel, Parameters] { type Out = ResModel }
    
    protected def inhabit_Type[Schema, TargetModel <: Model[_], Parameters, ResModel](
        f: List[Schema] => ResModel
    ): Aux[Schema, TargetModel, Parameters, ResModel] 
        = new IsValidSchema[Schema, TargetModel, Parameters] { 
            type Out = ResModel 
            def apply(dataset: List[Schema]) = f(dataset) 
    }

    implicit def target_model_is_json [
        Schema, TargetModel <: JSON[_], Parameters, 
        Repr <: HList
    ](
        implicit
        res_model: ValidJSON.Aux[Schema, Repr]
    ) = inhabit_Type[Schema, TargetModel, Parameters, JSON.Aux[Schema, Repr]]((dataset: List[Schema]) => res_model(dataset))

    implicit def target_model_is_relation [
        Schema, TargetModel <: Relation[_], Parameters, 
        Repr <: HList
    ](
        implicit
        res_model: ValidRelation.Aux[Schema, Repr]
    ) = inhabit_Type[Schema, TargetModel, Parameters, Relation.Aux[Schema, Repr]]((dataset: List[Schema]) => res_model(dataset))

    implicit def target_model_is_graph [
        Schema, TargetModel <: Graph[_,_,_], SourceID, DestID, 
        Repr <: HList
    ](
        implicit
        res_model: IsValidGraph.Aux[Schema, SourceID, DestID, Repr]
    ) = inhabit_Type[Schema, TargetModel, (SourceID, DestID), Graph.Aux[Schema, SourceID, DestID, Repr]]((dataset: List[Schema]) => res_model(dataset))
}