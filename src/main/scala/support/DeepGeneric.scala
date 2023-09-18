package pridwen.support

import shapeless.{HList, HNil, ::, LabelledGeneric, Lazy}
import shapeless.ops.hlist.{Prepend}
import shapeless.labelled.{FieldType => Field, field}

trait DeepGeneric[T] extends Serializable { 
    type Repr <: HList
    def to(t: T): Repr
    def from(r: Repr): T
}
trait LowPriorityDeepGeneric {
    type Aux[T, Repr0 <: HList] = DeepGeneric[T] { type Repr = Repr0 }
    protected def inhabit_Type[T, Repr0 <: HList](to0: T => Repr0)(from0: Repr0 => T): Aux[T, Repr0] = new DeepGeneric[T] {
        type Repr = Repr0
        def to(t: T) = to0(t)
        def from(r: Repr) = from0(r)
    }

    implicit def rule3[H, T <: HList, Repr0 <: HList](
        implicit
        d: DeepGeneric.Aux[T, Repr0]
    ) = inhabit_Type[H::T, H::Repr0](
        (t: H::T) => t.head :: d.to(t.tail)
    )(
        (r: H::Repr0) => r.head :: d.from(r.tail)
    )
}
object DeepGeneric extends LowPriorityDeepGeneric {
    def apply[T](implicit ok: DeepGeneric[T]): Aux[T, ok.Repr] = ok

    implicit def rule1[T, ReprT, Repr0 <: HList](
        implicit
        g: LabelledGeneric.Aux[T, ReprT],
        d: DeepGeneric.Aux[ReprT, Repr0]
    ) = inhabit_Type[T, Repr0](
        (t: T) => d.to(g.to(t))
    )(
        (r: Repr0) => g.from(d.from(r))
    )

    implicit def rule2[K, H, T <: HList, ReprH, Repr0 <: HList, Repr1 <: HList](
        implicit
        g: LabelledGeneric.Aux[H, ReprH],
        d1: Lazy[DeepGeneric.Aux[ReprH, Repr0]],
        d2: DeepGeneric.Aux[T, Repr1]
    ) = inhabit_Type[Field[K,H]::T, Field[K,Repr0]::Repr1](
        (t: Field[K,H]::T) => field[K](d1.value.to(g.to(t.head))) :: d2.to(t.tail)
    )(
        (r: Field[K,Repr0]::Repr1) => field[K](g.from(d1.value.from(r.head))) :: d2.from(r.tail)
    )

    implicit def rule4 = inhabit_Type[HNil, HNil](
        (t: HNil) => HNil
    )(
        (r: HNil) => HNil
    )
}