package pridwen.support

import shapeless.{HList, HNil, ::}



// Checks if type T is a subtype of one of the types contained in HLIst ListOfTypes. Output type is T's super type.
trait IsSubTypeOf[T, ListOfTypes <: HList] { type Out }
trait LowPriorityIsSubTypeOf {
  type Aux[T, ListOfTypes <: HList, SuperT] = IsSubTypeOf[T, ListOfTypes] { type Out = SuperT }

  protected def inhabit_Type[T, ListOfTypes <: HList, SuperT]: Aux[T, ListOfTypes, SuperT] 
    = new IsSubTypeOf[T, ListOfTypes] { type Out = SuperT }

  implicit def t_is_not_subtype_of_head [
    T, Head, Tail <: HList, 
    SuperT
  ](
    implicit
    check_in_tail: IsSubTypeOf.Aux[T, Tail, SuperT]
  ) = inhabit_Type[T, Head::Tail, SuperT]
}
object IsSubTypeOf extends LowPriorityIsSubTypeOf {
  def apply[T, ListOfTypes <: HList](implicit ok: IsSubTypeOf[T, ListOfTypes]): Aux[T, ListOfTypes, ok.Out] = ok

  implicit def t_is_subtype_of_head [
    T, Head >: T, Tail <: HList
  ] = inhabit_Type[T, Head::Tail, Head]
}