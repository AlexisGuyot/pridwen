package pridwen.support

import shapeless.{HList, HNil, ::, Typeable, Lazy}

import scala.reflect.runtime.universe.{TypeTag, WeakTypeTag}

import pridwen.support.{UPrepend, ToHList}
import pridwen.models.{Model}

object display {
    // To select and display one path-dependant type
    trait Show[T] extends Serializable {}
    //object Show{ def apply[T](implicit p: UPrepend[T, HNil]) = new Show[p.Out] {} }
    object Show{ def apply[T](implicit t: ToHList[T]) = new Show[t.Out] {} }

    def old_show[T](value: T)(implicit tag: TypeTag[T]): String = old_show(tag)
    def old_show[T](implicit tag: TypeTag[T]) = basic_format_output(tag.toString)

    def show[T : TypeTag](value: T): String = show("", value)
    def show[T](val_name: String, value: T)(implicit tag: TypeTag[T]): String = format_output(val_name + ": " + tag.toString())
    def show[T](val_name: String)(implicit tag: TypeTag[T]) = format_output(val_name + ": " + tag.toString())
    def show[T](implicit tag: TypeTag[T]) = format_output(tag.toString())
    def show(value: String): String = format_output(value)
    
    def show_model[S <: HList : TypeTag] = println(show[S])

    def print_operator(show_in: String, show_mid: String, show_out: String) = {
        println(format_show("In", show_in))
        println(format_show("Mid", show_mid))
        println(format_show("Out", show_out))
    }
    def print_operator(show_in: String, show_out: String) = {
        println(format_show("In", show_in))
        println(format_show("Out", show_out))
    }
    def print_operator[In <: HList, Mid <: HList, Out <: HList](op_name: String)(
        implicit
        print_in: PrintHList[In],
        print_mid: PrintHList[Mid],
        print_out: PrintHList[Out]
    ) = println(s"===========================\n[Print] ${op_name}\n===========================\nIn = ${print_in.apply}\n\nMid = ${print_mid.apply}\n\nOut = ${print_out.apply}\n===========================")

    def print_hlist[H <: HList](implicit print_hlist: PrintHList[H]): String = format_output(print_hlist.apply + " ")
    def old_print_hlist[H <: HList](implicit print_hlist: PrintHList[H]): String = print_hlist.apply

    def format_show(type_name: String, show_res: String): String = {
        val regex = """: pridwen\.support\.display\.Show\[(.+)\]""".r
        return regex.replaceAllIn(format(show_res), type_name + " = $1")
    }

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

        // Format field types.
        var tmp = ""
        do {
            tmp = res
            res = """FieldType\[Symbol with Tagged\[String\("([^"]+)"\)\],((?:(?:[\w()]+| -> )(?: :: )?)+(?:HNil)?)\]|FieldType\[Symbol with Tagged\[String\("([^"]+)"\)\],((?:(?:(\R))(?: :: )?)+(?:HNil)?)\]""".r.replaceAllIn(res, "($1 -> $2)")
        } while(res != tmp)

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
    private val format = (s: String) => s.trim().replaceAll("\\n", "").replaceAll("\\t", "") 

    abstract class PrintHList[H <: HList] { def apply(): String }
    trait LowPriorityPrintHList {
        protected def inhabit_Type[H <: HList](f: Unit => String): PrintHList[H] = new PrintHList[H] { def apply(): String = f() }
        protected def format_mid(s: String): String = """Product\[([^\]]+),([^\]]+)\]""".r.replaceAllIn(format(s), "Product[\n\t$1,\n\t$2\n]")

        implicit def print_hlist[H, T <: HList](
            implicit
            tag: TypeTag[H],
            p: PrintHList[T]
        ) = inhabit_Type[H::T](_ => s"${format_mid(show[H](tag))} :: ${p.apply}")
    }
    object PrintHList extends LowPriorityPrintHList {
        def apply[H <: HList](format: String => String)(implicit ok: PrintHList[H]): PrintHList[H] = ok

        implicit def print_hnil = inhabit_Type[HNil](_ => show[HNil])
        implicit def print_sub_hlist[H <: HList, T <: HList](
            implicit
            p1: Lazy[PrintHList[H]],
            p2: PrintHList[T]
        ) = inhabit_Type[H::T](_ => s"${p1.value.apply} :: ${p2.apply}")
        implicit def print_product[A <: HList, B <: HList, T <: HList](
            implicit
            pa: PrintHList[A],
            pb: PrintHList[B],
            pt: PrintHList[T]
        ) = inhabit_Type[Product2[A, B]::T](_ => s"Product[\n\t${pa.apply},\n\t${pb.apply}\n] :: ${pt.apply}")
    }
}

