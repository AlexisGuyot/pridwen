package pridwen.models.aux 

import shapeless.{HList, HNil, ::, Witness}
import shapeless.ops.hlist.{Prepend}
import shapeless.labelled.{FieldType => Field, field}

import pridwen.support.{ToHList}
import pridwen.support.functions.{getFieldValue}

/* trait Transformation
trait Add extends Transformation
case object Add extends Add
trait Update extends Transformation
case object Update extends Update */

object transformations {
    trait Add[P <: HList, AN, AT]
    def Add[T]: AddBuilder[T] = new AddBuilder(true)
    class AddBuilder[T](private val dummy: Boolean) extends AnyVal {
        def apply[P <: HList](p: P, n: Witness): Add[P, n.T, T] = new Add[P, n.T, T] {}
    }

    trait Update[P <: HList, AN, AT]
    def Update[T]: UpdateBuilder[T] = new UpdateBuilder(true)
    class UpdateBuilder[T](private val dummy: Boolean) extends AnyVal {
        def apply[P <: HList](p: P, n: Witness): Update[P, n.T, T] = new Update[P, n.T, T] {}
    }
}

import transformations._


trait UpdateSchema[S <: HList, T] { type Out <: HList }
object UpdateSchema {
    def apply[S <: HList, T](implicit ok: UpdateSchema[S, T]): Aux[S, T, ok.Out] = ok
    type Aux[S <: HList, T, Out0 <: HList] = UpdateSchema[S, T] { type Out = Out0 }
    protected def inhabit_Type[S <: HList, T, Out0 <: HList]: Aux[S, T, Out0] = new UpdateSchema[S, T] { type Out = Out0 }

    implicit def add[S <: HList, P <: HList, AN, AT, Out0 <: HList]
    (
        implicit
        a: AddAtt.Aux[S, P, AN, AT, Out0]
    ) = inhabit_Type[S, Add[P, AN, AT], Out0]

    implicit def update[S <: HList, P <: HList, AN, AT, Out0 <: HList](
        implicit
        a: ReplaceAtt.Aux[S, P, AN, AT, Out0]
    ) = inhabit_Type[S, Update[P, AN, AT], Out0]

    implicit def multiple_transfo[S <: HList, HT, TT <: HList, Out0 <: HList, Out1 <: HList]
    (
        implicit
        u1: UpdateSchema.Aux[S, HT, Out0],
        u2: UpdateSchema.Aux[Out0, TT, Out1]
    ) = inhabit_Type[S, HT::TT, Out1]

    implicit def no_transo[S <: HList] = inhabit_Type[S, HNil, S]
}