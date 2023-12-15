package pridwen.support

import scala.reflect.runtime.universe.{TypeTag}
import scala.reflect.{ClassTag}



trait PrintType[T] { def apply(): String }
trait LowPriorityPrintType {
    
    protected def inhabit_Type[T](
        tag: String
    ): PrintType[T] 
        = new PrintType[T] { def apply() = tag }

    implicit def default[T](implicit tag: ClassTag[T]) = inhabit_Type[T](s"${tag.toString}")
}
object PrintType extends LowPriorityPrintType {
    def apply[T](implicit ok: PrintType[T]): PrintType[T] = ok

    implicit def has_typetag[T](implicit tag: TypeTag[T]) = inhabit_Type[T](s"${tag.tpe}")

    implicit def composite_one_arg[T[_], A](
        implicit 
        tagT: ClassTag[T[A]],
        tagA: PrintType[A]
    ) = inhabit_Type[T[A]](s"${tagT.toString}[${tagA.apply}]")

    implicit def composite_two_arg[T[_,_], A, B](
        implicit 
        tagT: ClassTag[T[A, B]],
        tagA: PrintType[A],
        tagB: PrintType[B]
    ) = inhabit_Type[T[A, B]](s"${tagT.toString}[${tagA.apply},${tagB.apply}]")

    implicit def composite_three_arg[T[_,_,_], A, B, C](
        implicit 
        tagT: ClassTag[T[A, B, C]],
        tagA: PrintType[A],
        tagB: PrintType[B],
        tagC: PrintType[C]
    ) = inhabit_Type[T[A, B, C]](s"${tagT.toString}[${tagA.apply},${tagB.apply},${tagC.apply}]")
}