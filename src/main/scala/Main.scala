package pridwen

object Main extends App {  
    import shapeless.{::, HNil, Witness => W, LabelledGeneric}
    import shapeless.labelled.{FieldType => Field, field}
    import pridwen.models.{JSON, Relation}
    import pridwen.models.aux.{SelectAtt, As}
    import pridwen.support.display._
    import pridwen.support.{DeepGeneric}
    import pridwen.operators.predefined.ops._
    
    type InputSchema1 = Field[W.`'user`.T, Field[W.`'id`.T, Long] :: HNil] :: Field[W.`'retweeted_status`.T, Field[W.`'user`.T, Field[W.`'id`.T, Long] :: HNil] :: HNil] :: HNil
    case class User(id: Long, name: String)
    case class RetweetedStatus(user: User)
    case class CC_InputSchema1(user: User, retweeted_status: RetweetedStatus)
    case class CC_InputSchema2(user: User)

    println(show(SelectAtt[InputSchema1, W.`'user`.T :: HNil]))

    val gen1 = LabelledGeneric[CC_InputSchema1]
    println(show(gen1))
    val gen2 = DeepGeneric[CC_InputSchema1]
    println(show(gen2))

    val dataset_cc = List(
        CC_InputSchema1(User(1268486802949767200L, "A"), RetweetedStatus(User(277430850L, "B"))),
        CC_InputSchema1(User(1268486802949767200L, "A"), RetweetedStatus(User(277430850L, "B"))),
        CC_InputSchema1(User(1268486302459767200L, "C"), RetweetedStatus(User(277430850L, "B"))),        
    )
    println(show(dataset_cc))
    val dataset_hlist = dataset_cc.map(cc => gen2.to(cc))
    //println(show(dataset_hlist))
    println(dataset_hlist)

    val dataset_json = JSON[gen2.Repr](dataset_hlist)
    val build_graph_rt = constructGraph(
        dataset_json, 
        //W('user) :: field[W.`'id`.T](0L) :: HNil, 
        //W('retweeted_status) :: W('user) :: field[W.`'id`.T](0L) :: HNil, 
        W('user) :: W('id) :: HNil, 
        W('retweeted_status) :: W('user) :: W('id) :: HNil, 
        HNil,
        HNil
    )
    //println(show(build_graph_rt))

    println("Hello World") 
}