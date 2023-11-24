package pridwen.models

import shapeless.{HList, Witness}

import scala.collection.parallel.CollectionConverters._

trait Model { 
    type Schema <: HList 
    val data: List[Schema]

    protected def convert_to_repr[InputSchema, Repr <: HList](dataset: List[InputSchema], toRepr: InputSchema => Repr): List[Repr] = dataset.par.map(schema => toRepr(schema)).toList
}

object Model {
    trait As[S, M <: Model] { type T ; def apply(dataset: List[S]): T }
    object As {
        type Aux[S, M <: Model, T0] = As[S, M] { type T = T0 }
        
        private def inhabit_Type[S, M <: Model, T0](
            f: List[S] => T0
        ): Aux[S, M, T0] = new As[S, M] {
            type T = T0
            def apply(dataset: List[S]): T = f(dataset)
        }

        implicit def as_json[S](
            implicit
            json: JSON.ValidSchema[S]
        ) = inhabit_Type[S, JSON, json.T]((dataset: List[S]) => json(dataset))

        implicit def as_relation[S](
            implicit
            relation: Relation.ValidSchema[S]
        ) = inhabit_Type[S, Relation, relation.T]((dataset: List[S]) => relation(dataset))

        implicit def as_graph[S, Schema <: HList, SourceID, DestID, SourceSchema <: HList, DestSchema <: HList](
            implicit
            graph: Graph.ValidSchema[S, SourceID, DestID]
        ) = inhabit_Type[S, Graph.Aux[Schema, SourceID, DestID, SourceSchema, DestSchema], graph.T](
            (dataset: List[S]) => graph(dataset)
        )
    }
}