package pridwen.old.models

import shapeless.{HList, HNil, ::}
import shapeless.labelled.{FieldType}
import shapeless.ops.hlist.{IsHCons}

//trait Model[S <: HList]

trait Model { type Schema <: HList ; val data: List[Schema] }
/* object Model {
    trait Infer[S] { type Out <: Model }
    trait ModelHierarchy0 {
        type Aux[S, Out0 <: Model] = Infer[S] { type Out = Out0 }
        protected def inhabit_Type[Out0 <: Model](m: Out0) = new Infer[m.Schema] { type Out = Out0 }
    }
    trait ModelHierarchy1 extends ModelHierarchy0 {
        /* implicit def default[S <: HList](
            implicit
            i: NoModel.IsConform[S]
        ) = inhabit_Type(NoModel[S])    */   
    }
    trait ModelHierarchy2 extends ModelHierarchy1 {
        /* implicit def s_is_keyvalue[S <: FieldType[_,_]](
            implicit
            i: KeyValue.IsConform[S::HNil]
        ) = inhabit_Type(KeyValue[S::HNil]) */
    }
    trait ModelHierarchy3 extends ModelHierarchy2 {
        implicit def s_is_json[S <: HList, M <: Model](
            implicit
            i: JSON.IsConform[S]
        ) = inhabit_Type(JSON[S])
    }
    object Infer extends ModelHierarchy3 {
        def apply[S <: HList](implicit ok: Infer[S]): Aux[S, ok.Out] = ok

        implicit def s_is_relational[S <: HList](
            implicit
            i: Relation.IsConform[S]
        ) = inhabit_Type(Relation[S])

        implicit def s_is_graph[S <: HList]
        (
            implicit
            i: Graph.IsConform[S]
        ) = inhabit_Type(Graph[S])

        /* implicit def s_is_multiple[S <: HList, S1 <: HList, S2 <: HList, M1 <: Model, M2 <: Model]
        (
            implicit
            h: IsHCons.Aux[S, Product2[S1, S2], HNil],
            m1: Infer.Aux[S1, M1],
            m2: Infer.Aux[S2, M2]
        ) = inhabit_Type(Multiple2[S, M1, M2]) */

        implicit def s_is_model[M <: Model] = new Infer[M::HNil] { type Out = M }
    }
} */