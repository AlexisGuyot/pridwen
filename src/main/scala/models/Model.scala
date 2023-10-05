package pridwen.models

import shapeless.{HList, HNil, ::, Witness}
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
    type JSON = pridwen.models.JSON[HNil]
    type Relation = pridwen.models.Relation[HNil]
    type Graph[SourceID, DestID] = pridwen.models.Graph[HNil, SourceID, DestID]

    // Dummy model instances with empty schemas
    def JSON(implicit json: IsValidJSON[HNil]) = json(List(HNil))
    def Relation(implicit relation: IsValidRelation[HNil]) = relation(List(HNil))
    def Graph(source_id: Witness, dest_id: Witness)(implicit graph: IsValidGraph[(Field[source_id.T, Int] :: HNil) :: (Field[dest_id.T, Int] :: HNil) :: HNil, source_id.T, dest_id.T]) = graph(List((field[source_id.T](0)::HNil) :: (field[dest_id.T](0)::HNil) :: HNil))
}