package pridwen.operators

import pridwen.support.{PrintType}
import pridwen.models.{Model}
import pridwen.schemaop.{PrintSchema}

object display {
    def show_dataset[M <: Model](
        dataset: M, 
        name: String
    )(
        implicit
        schema: PrintSchema[dataset.Schema]
    ): Unit = {
        val m: String = dataset match {
            case _: pridwen.models.Relation => "Relation"
            case _: pridwen.models.JSON => "JSON"
            case _: pridwen.models.Graph => "Graph"
            case _ => "Model"
        }
        println(s"============= ${name}\n")
        println(s"Model: ${m}\n")
        println(s"Schema: \n${schema()}")
        if(dataset.data.size < 10) println(s"Data: ${dataset.data}\n")
        println("=======================================\n")
    }

    def show_dataset_nomodel[T](
        dataset: T, 
        name: String,
        show_data: Boolean = true
    )(
        implicit
        print_dataset: PrintType[T]
    ): Unit = {
        println(s"============= ${name}\n")
        println(s"Type: ${print_dataset.apply}\n")
        if(show_data) println(s"Value: ${dataset}\n")
        println("=======================================\n")
    }
}

