package pridwen.models

import shapeless.{HList, HNil, ::, Witness => W}
import shapeless.labelled.{FieldType => Field, field}

import pridwen.models.aux.{SelectField}



abstract class Model[Schema](dataset: List[Schema]) { 
    type Repr <: HList 
    def toRepr(schema: Schema): Repr 

    val data: List[Repr] = dataset.map(schema => toRepr(schema))

    def get[Path, FieldName, FieldType](
        path: Path, 
        filter: Repr => Boolean = (_ => true)
    )(
        implicit 
        select_field: SelectField.Aux[Repr, Path, FieldName, FieldType]
    ): List[Field[FieldName,FieldType]] = { 
        val result = scala.collection.mutable.ListBuffer.empty[Field[FieldName,FieldType]] 
        data.foreach(repr => if(filter(repr)) select_field(repr) +=: result)
        result.to(List) 
    }
}
object Model {
    // Dummy model instances with empty schemas
    def JSON(implicit json: IsValidJSON[HNil]) = json(List(HNil))
    def Relation(implicit relation: IsValidRelation[HNil]) = relation(List(HNil))
    def Graph(implicit graph: IsValidGraph[(Field[W.`'id`.T, Int] :: HNil) :: (Field[W.`'id`.T, Int] :: HNil) :: HNil, W.`'id`.T, W.`'id`.T]) = graph(List((field[W.`'id`.T](0)::HNil) :: (field[W.`'id`.T](0)::HNil) :: HNil))
}