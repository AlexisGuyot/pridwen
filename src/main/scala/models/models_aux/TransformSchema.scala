package pridwen.models.aux 

import shapeless.{HList, HNil, ::, Witness}

trait TransformSchema[Schema <: HList, Transformation] { type Out <: HList }
object TransformSchema {
    type Aux[Schema <: HList, Transformation, New_Schema <: HList] = TransformSchema[Schema, Transformation] { type Out = New_Schema }
    
    protected def inhabit_Type[Schema <: HList, Transformation, New_Schema <: HList]: Aux[Schema, Transformation, New_Schema] = new TransformSchema[Schema, Transformation] { type Out = New_Schema }

    implicit def transfo_is_add [
        Schema <: HList, Path <: HList, Field_Name, Field_Type, 
        New_Schema <: HList
    ](
        implicit
        add_field: AddField.Aux[Schema, Path, Field_Name, Field_Type, New_Schema]
    ) = inhabit_Type[Schema, Add[Path, Field_Name, Field_Type], New_Schema]

    implicit def transfo_is_update [
        Schema <: HList, Path <: HList, Field_Name, Field_Type, 
        New_Schema <: HList
    ](
        implicit
        replace_field: ReplaceField.Aux[Schema, Path, Field_Name, Field_Type, New_Schema]
    ) = inhabit_Type[Schema, Update[Path, Field_Name, Field_Type], New_Schema]

    implicit def multiple_transfo [
        Schema <: HList, Transfo, OtherTransfo <: HList, 
        New_Schema1 <: HList, New_Schema2 <: HList
    ](
        implicit
        u1: TransformSchema.Aux[Schema, Transfo, New_Schema1],
        u2: TransformSchema.Aux[New_Schema1, OtherTransfo, New_Schema2]
    ) = inhabit_Type[Schema, Transfo::OtherTransfo, New_Schema2]

    implicit def no_transfo[Schema <: HList] = inhabit_Type[Schema, HNil, Schema]




    trait Add[Path <: HList, Field_Name, Field_Type]
    def Add[Field_Type]: AddBuilder[Field_Type] = new AddBuilder(true)
    class AddBuilder[Field_Type](private val dummy: Boolean) extends AnyVal {
        def apply[Path <: HList](path: Path, att_name: Witness): Add[Path, att_name.T, Field_Type] = new Add[Path, att_name.T, Field_Type] {}
    }

    trait Update[Path <: HList, New_Name, New_Type]
    def Update[New_Type]: UpdateBuilder[New_Type] = new UpdateBuilder(true)
    class UpdateBuilder[New_Type](private val dummy: Boolean) extends AnyVal {
        def apply[Path <: HList](path: Path, new_name: Witness): Update[Path, new_name.T, New_Type] = new Update[Path, new_name.T, New_Type] {}
    }
}