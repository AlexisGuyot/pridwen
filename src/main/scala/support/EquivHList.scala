package pridwen.support

import shapeless.{HList, ::, HNil}

trait EquivHList[L1 <: HList, L2 <: HList] { def apply(l: L1): L2 }
object EquivHList {
    def apply[L1 <: HList, L2 <: HList](implicit ok: EquivHList[L1, L2]): EquivHList[L1, L2] = ok
    private def inhabit_Type[L1 <: HList, L2 <: HList](f: L1 => L2): EquivHList[L1, L2] = new EquivHList[L1, L2] { def apply(l: L1) = f(l) }

    implicit def recurse_lists[H, T1 <: HList, T2 <: HList](
        implicit
        e: EquivHList[T1, T2]
    ) = inhabit_Type[H::T1, H::T2](
        (l: H::T1) => l.head :: e(l.tail)
    )

    implicit def stop_recursion = inhabit_Type[HNil, HNil](
        (l: HNil) => HNil
    )
}