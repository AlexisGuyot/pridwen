package pridwen.support

import shapeless.{HList, HNil, ::}
import shapeless.ops.hlist.{Reverse, IsHCons}

trait ReducePath[Path <: HList] { type Out <: HList ; def apply(p: Path): Out }
object ReducePath {
    def apply[Path <: HList](implicit ok: ReducePath[Path]): Aux[Path, ok.Out] = ok
    type Aux[Path <: HList, ReducedPath <: HList] = ReducePath[Path] { type Out = ReducedPath }

    protected def inhabit_Type[Path <: HList, ReducedPath <: HList](
        f: Path => ReducedPath
    ): Aux[Path, ReducedPath] 
        = new ReducePath[Path] { type Out = ReducedPath ; def apply(p: Path): Out = f(p) } 

    implicit def path_is_not_empty [
        Head, Tail <: HList, 
        ReversedPath <: HList, ReversedHead, ReversedTail <: HList, 
        ReducedPath <: HList
    ](
        implicit
        reverse_path: Reverse.Aux[Head::Tail, ReversedPath],
        decompose_reversed: IsHCons.Aux[ReversedPath, ReversedHead, ReversedTail],
        reverse_back: Reverse.Aux[ReversedTail, ReducedPath]
    ) = inhabit_Type[Head::Tail, ReducedPath](
        (p: (Head::Tail)) => reverse_back(reverse_path(p).tail)
    )

    implicit def path_is_empty = inhabit_Type[HNil, HNil]((p: HNil) => HNil)
}