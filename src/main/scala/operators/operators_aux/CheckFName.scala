package pridwen.operators.aux

import shapeless.{Witness => W}

trait CheckFName[FN1, FN2] { type Out1 ; type Out2 }
trait LowPriorityCheckFName {
    type Aux[FN1, FN2, Out10, Out20] = CheckFName[FN1, FN2] { type Out1 = Out10 ; type Out2 = Out20 }
    protected def inhabit_Type[FN1, FN2, Out10, Out20]: Aux[FN1, FN2, Out10, Out20] = new CheckFName[FN1, FN2] { type Out1 = Out10 ; type Out2 = Out20 }
    
    implicit def different_names[FN1, FN2] = inhabit_Type[FN1, FN2, FN1, FN2]
}
object CheckFName extends LowPriorityCheckFName {
    def apply[FN1, FN2](implicit ok: CheckFName[FN1, FN2]): Aux[FN1, FN2, ok.Out1, ok.Out2] = ok

    implicit def same_name[FN] = inhabit_Type[FN, FN, W.`'left_key`.T, W.`'right_key`.T]
}