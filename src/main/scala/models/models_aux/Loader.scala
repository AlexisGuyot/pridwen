package pridwen.models.aux

import pridwen.models._

trait Loader[M <: Model[_], S, P] { type Out }
object Loader {
    def apply[M <: Model[_], S, P](dataset: List[S])(implicit ok: Loader[M, S, P]): Aux[M, S, P, ok.Out] = ok
    type Aux[M <: Model[_], S, P, Out0] = Loader[M, S, P] { type Out = Out0 }
    protected def inhabit_Type[M <: Model[_], S, P, Out0](m: Out0): Aux[M, S, P, Out0] = new Loader[M, S, P] { type Out = Out0 }
}
