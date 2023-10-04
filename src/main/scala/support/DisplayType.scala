package pridwen.support

import shapeless.{HList}

import scala.reflect.runtime.universe.{TypeTag}

import pridwen.support.{ToHList, PrintType}
import pridwen.models.{Model}
import pridwen.models.aux.{PrintSchema}



object display {
    def show_dataset[M <: Model[_]](
        dataset: M, 
        name: String
    )(
        implicit
        schema: PrintSchema[dataset.Repr]
    ): Unit = {
        val m: String = dataset match {
            case _: pridwen.models.Relation[_] => "Relation"
            case _: pridwen.models.JSON[_] => "JSON"
            case _: pridwen.models.Graph[_,_,_] => "Graph"
            case _ => "Model"
        }
        println(s"============= ${name}\n")
        println(s"Model: ${m}\n")
        println(s"Schema: \n${schema()}")
        println(s"Data: ${dataset.data}\n")
        println("=======================================\n")
    }

    def show_dataset_nomodel[T](
        dataset: T, 
        name: String
    )(
        implicit
        print_dataset: PrintType[T]
    ): Unit = {
        println(s"============= ${name}\n")
        println(s"Type: ${print_dataset.apply}\n")
        println(s"Value: ${dataset}\n")
        println("=======================================\n")
    }

    trait Show[T] extends Serializable {}
    object Show{ def apply[T](implicit t: ToHList[T]) = new Show[t.Out] {} }
    def show[T](value: T)(implicit tag: TypeTag[T]): String = format_output(tag.tpe.toString)
    def show[T](implicit tag: TypeTag[T]): String = format_output(tag.tpe.toString)
    def show(value: String): String = format_output(value)

    def basic_format_output(s: String): String =
        s.replace("Main.", "")
        .replace("TypeTag[", "")
        .replace("types.", "")
        .replace("shapeless.", "")
        .replace("labelled.", "")
        .replace("tag.", "")
        .replace("reflect.runtime.universe.", "")
        .replace("pridwen.operators.", "")
        .dropRight(1)

    def format_output(s: String) = {
        var res = basic_format_output(s).replace("schema.", "").replace("HList :: HNil", "HList")
        var tmp = ""

        // Format not nested field types.
        res = """([a-zA-Z0-9]+) with KeyTag\[Symbol with Tagged\[String\("([\S]+)"\)\],([a-zA-Z0-9]+)\]""".r.replaceAllIn(res, """$2 -> $1""")

        // Format nested field types.
        do {
            tmp = res
            for(m <- """with KeyTag\[Symbol with Tagged\[String\("([\S]+)"\)\],((?:(?:(?:[a-zA-Z0-9\(\)]+)(?: -> [a-zA-Z0-9\(\)]+)+)(?:(?: :: (?:(?:(?:[a-zA-Z0-9\(\)]+)(?: -> [a-zA-Z0-9\(\)]+)+)|HList))*)*|\((?:[^\]]+),(?:[^\]]+)\)|Fusion\[(?:[^\]]+),(?:[^\]]+)\])(?: :: (?:HNil|HList)\)*)*)\]""".r("fname", "ftype").findAllMatchIn(res)) {
                res = (s"${escape(m.group("ftype"))}" + """ with KeyTag\[Symbol with Tagged\[String\("(""" + s"${m.group("fname")}" + """)"\)\],(""" + s"${escape(m.group("ftype"))}" + """)\]""").r.replaceAllIn(res, """$1 -> ($2)""")
            }
        } while(res != tmp)

        // Format unnamed nested field types.
        do {
            tmp = res
            for(m <- """with KeyTag\[Symbol,((?:(?:(?:[a-zA-Z0-9\(\)]+)(?: -> [a-zA-Z0-9\(\)]+)+)(?:(?: :: (?:(?:(?:[a-zA-Z0-9\(\)]+)(?: -> [a-zA-Z0-9\(\)]+)+)|HList))*)*|Join\[(?:[^\]]+),(?:[^\]]+)\]|Fusion\[(?:[^\]]+),(?:[^\]]+)\])(?: :: (?:HNil|HList)\)*)*)\]""".r("ftype").findAllMatchIn(res)) {
                res = (s"${escape(m.group("ftype"))}" + """ with KeyTag\[Symbol,(""" + s"${escape(m.group("ftype"))}" + """)\]""").r.replaceAllIn(res, """_ -> ($1)""")
            }
        } while(res != tmp)

        // Format nested field types with generic value types.
        do {
            tmp = res
            for(m <- """with KeyTag\[Symbol with Tagged\[String\("([^\]]+)"\)\],(\S+)\]""".r("fname", "ftype").findAllMatchIn(res)) {
                res = (s"${escape(m.group("ftype"))}" + """ with KeyTag\[Symbol with Tagged\[String\("(""" + s"${m.group("fname")}" + """)"\)\],(""" + s"${escape(m.group("ftype"))}" + """)\]""").r.replaceAllIn(res, """$1 -> ($2)""")
            }
        } while(res != tmp)

        // Format models
        do {
            tmp = res
            res = """Graph\{(.+); type Nodes = (?:[^}]+)\}""".r.replaceAllIn(res, """Graph{$1}""")
            res = """pridwen\.models\.((?:\w+)(?:\[(?:.+)\])?)\{type Schema = ([^}]+)\}""".r.replaceAllIn(res, "$1{$2}")
        } while(res != tmp)
        res = res.replace("pridwen.models.", "")
        res = """Multiple2\[([^,]+),(.+)\]\{(?:type Schema = )?Product(?:2)?\[(\1) :: HNil,(\2) :: HNil\] :: HNil\}""".r.replaceAllIn(res, "Multiple[$1, $2]")
        res = """Multiple2\[(\w+\{(.+)\}),(\w+\{(.+)\})\]\{(?:type Schema = )?Product(?:2)?\[\2,\4\] :: HNil\}""".r.replaceAllIn(res, "Multiple[$1, $3]")

        // Format field types.
        tmp = ""
        do {
            tmp = res
            res = """FieldType\[Symbol with Tagged\[String\("([^"]+)"\)\],((?:(?:[\w()]+| -> )(?: :: )?)+(?:HNil)?)\]|FieldType\[Symbol with Tagged\[String\("([^"]+)"\)\],((?:(?:(\R))(?: :: )?)+(?:HNil)?)\]""".r.replaceAllIn(res, "$1 -> ($2)")
        } while(res != tmp)


        // Format auxiliary types
        res = """Add\[([^\]]+)\]""".r.replaceAllIn(res, """Add($1)""")
        res = """\((\w[^,]+), (\w+ -> (?:\((?:[\w\s\-\>]|(?:(\R)))+\))|(?:\w+ -> \w+))\)""".r.replaceAllIn(res, """Join($1 = $2)""")
        //res = """Product2\[([^\]]+),([^\]]+)\]""".r.replaceAllIn(res, "Product[\t$1,\t$2\t]")
        // Indentation
        //res = """(\{|,|; )""".r.replaceAllIn(res, "$1\n\t")
        //res = """(\t*)(\})""".r.replaceAllIn(res, "\n$1$2")
        
        res
    }

    private def escape(s: String): String = s.replace("(", """\(""").replace(")", """\)""").replace("{", """\{""").replace("}", """\}""").replace("[", """\[""").replace("]", """\]""")
}

