package pridwen

object Main extends App {  
    import shapeless.{::, HNil, Witness => W, LabelledGeneric}
    import shapeless.labelled.{FieldType => Field}
    import pridwen.models.{JSON}
    import pridwen.models.aux.{SelectAtt}
    import pridwen.support.display._
    import pridwen.support.{DeepGeneric}
    
    type InputSchema1 = Field[W.`'user`.T, Field[W.`'id`.T, Long] :: HNil] :: Field[W.`'retweeted_status`.T, Field[W.`'user`.T, Field[W.`'id`.T, Long] :: HNil] :: HNil] :: HNil
    case class User(id: Long)
    case class RetweetedStatus(user: User)
    case class CC_InputSchema1(user: User, retweeted_status: RetweetedStatus)
    case class CC_InputSchema2(user: User)

    val gen1 = LabelledGeneric[CC_InputSchema1]
    println(show(gen1))
    val gen2 = DeepGeneric[CC_InputSchema1]
    println(show(gen2))

    val dataset_cc = List(
        CC_InputSchema1(User(1268486802949767200L), RetweetedStatus(User(277430850L))),
        CC_InputSchema1(User(1268486802949767200L), RetweetedStatus(User(277430850L))),
        CC_InputSchema1(User(1268486302459767200L), RetweetedStatus(User(277430850L))),        
    )
    println(show(dataset_cc))
    val dataset_hlist: List[InputSchema1] = dataset_cc.map(cc => gen2.to(cc))
    println(show(dataset_hlist))
    println(dataset_hlist)

    val dataset_json = JSON[InputSchema1](dataset_hlist)

    println("Hello World") 
}