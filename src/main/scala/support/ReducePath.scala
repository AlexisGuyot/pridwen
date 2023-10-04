package pridwen.support

import shapeless.{HList, HNil, ::}
import shapeless.ops.hlist.{Reverse, IsHCons}



trait ReducePath[Path <: HList] { type Out <: HList }
object ReducePath {
    def apply[Path <: HList](implicit ok: ReducePath[Path]): Aux[Path, ok.Out] = ok
    type Aux[Path <: HList, ReducedPath <: HList] = ReducePath[Path] { type Out = ReducedPath }

    protected def inhabit_Type[Path <: HList, ReducedPath <: HList]: Aux[Path, ReducedPath] 
        = new ReducePath[Path] { type Out = ReducedPath } 

    implicit def path_is_not_empty [
        Head, Tail <: HList, 
        ReversedPath <: HList, ReversedHead, ReversedTail <: HList, 
        ReducedPath <: HList
    ](
        implicit
        reverse_path: Reverse.Aux[Head::Tail, ReversedPath],
        decompose_reversed: IsHCons.Aux[ReversedPath, ReversedHead, ReversedTail],
        reverse_back: Reverse.Aux[ReversedTail, ReducedPath]
    ) = inhabit_Type[Head::Tail, ReducedPath]

    implicit def path_is_empty = inhabit_Type[HNil, HNil]
}