package pridwen

object Main extends App {  
    import shapeless.{::, HNil, Witness => W, LabelledGeneric}
    import shapeless.labelled.{FieldType => Field, field}
    import pridwen.models.alt._
    import pridwen.models.aux.{SelectAtt, As, SelectManyAtt, SelectSiblings}
    import pridwen.support.display._
    import pridwen.support.{DeepGeneric}
    import pridwen.operators.predefined.ops._
    
    type InputSchema1 = Field[W.`'user`.T, Field[W.`'id`.T, Long] :: HNil] :: Field[W.`'retweeted_status`.T, Field[W.`'user`.T, Field[W.`'id`.T, Long] :: HNil] :: HNil] :: HNil
    case class User(id: Long, name: String)
    case class RetweetedStatus(user: User)
    case class CC_InputSchema1(user: User, retweeted_status: RetweetedStatus)
    case class CC_InputSchema2(user: User)
    //val gen2 = DeepGeneric[CC_InputSchema1]

    val dataset_cc = List(
        CC_InputSchema1(User(1268486802949767200L, "A"), RetweetedStatus(User(277430850L, "B"))),
        CC_InputSchema1(User(1268486802949767200L, "A"), RetweetedStatus(User(277430850L, "B"))),
        CC_InputSchema1(User(1268486302459767200L, "C"), RetweetedStatus(User(277430850L, "B"))),        
    )
    //val dataset_hlist = dataset_cc.map(cc => gen2.to(cc))
    //val input_model = JSON[CC_InputSchema1]
    //val dataset_json = JSON.load(JSON[CC_InputSchema1])(dataset_cc)

    val graph_rt = constructGraph(
        JSON[CC_InputSchema1](dataset_cc), 
        W('user) :: W('id) :: HNil, 
        W('retweeted_status) :: W('user) :: W('id) :: HNil, 
        (W('user) :: W('name) :: HNil) :: HNil,
        As(W('test), W('retweeted_status) :: W('user) :: W('name) :: HNil) :: HNil,
        HNil
    )
    println("\nSchema graph_rt:")
    println(show(graph_rt))
    println("\nGraph_rt:")
    println(graph_rt.data)
    println()

    /* println(show_op("build_graph_rt", build_graph_rt))

    val graph_rt = build_graph_rt(dataset_json)
    println("\nSchema graph_rt:")
    println(show(graph_rt))
    println("\nGraph_rt:")
    println(graph_rt.data)
    println() */

    pridwen.models.alt.JSON[CC_InputSchema1](dataset_cc)
    println(show(SelectSiblings[InputSchema1, W.`'retweeted_status`.T :: W.`'user`.T :: W.`'id`.T :: HNil]))
}