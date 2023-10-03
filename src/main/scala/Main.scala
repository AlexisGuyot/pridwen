package pridwen

object Main extends App {  
    import shapeless.{HList, ::, HNil, Witness => W, LabelledGeneric}
    import shapeless.labelled.{FieldType => Field, field}
    import pridwen.models._
    import pridwen.models.aux.{SelectAtt, As, SelectManyAtt, SelectSiblings}
    import pridwen.models.aux.transformations.{Add, Update}
    import pridwen.support.display._
    import pridwen.support.{DeepGeneric}
    import pridwen.support.functions.{get}
    import pridwen.operators.predefined.construct._
    import pridwen.operators.predefined.join._
    import pridwen.operators.predefined.transform._
    
    type InputSchema1 = Field[W.`'user`.T, Field[W.`'id`.T, Long] :: HNil] :: Field[W.`'retweeted_status`.T, Field[W.`'user`.T, Field[W.`'id`.T, Long] :: HNil] :: HNil] :: HNil
    case class User(id: Long, name: String)
    case class RetweetedStatus(user: User)
    case class QuotedStatus(user: User)
    case class CC_InputSchema1(user: User, retweeted_status: RetweetedStatus)
    case class CC_InputSchema2(user: User, quoted_status: QuotedStatus)
    //val gen2 = DeepGeneric[CC_InputSchema1]

    val dataset_cc1 = List(
        CC_InputSchema1(User(1268486802949767200L, "A"), RetweetedStatus(User(277430850L, "B"))),
        CC_InputSchema1(User(1268486802949767200L, "A"), RetweetedStatus(User(277430850L, "B"))),
        CC_InputSchema1(User(1268486302459767200L, "C"), RetweetedStatus(User(277430850L, "B"))),        
    )
    val dataset_cc2 = List(
        CC_InputSchema2(User(1268486802949767200L, "A"), QuotedStatus(User(277430850L, "B"))),
        CC_InputSchema2(User(277430850L, "B"), QuotedStatus(User(1268486802949767200L, "A"))),
        CC_InputSchema2(User(1268486302459767200L, "C"), QuotedStatus(User(1268486802949767200L, "A"))),        
    )
    //val dataset_hlist = dataset_cc.map(cc => gen2.to(cc))
    //val input_model = JSON[CC_InputSchema1]
    //val dataset_json = JSON.load(JSON[CC_InputSchema1])(dataset_cc)

    /* val graph_rt = constructGraph(
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

    val rel_dataset = Relation[Field[W.`'id`.T, Long] :: Field[W.`'community`.T, Int] :: HNil](List(
        field[W.`'id`.T](1268486802949767200L) :: field[W.`'community`.T](1) :: HNil,
        field[W.`'id`.T](1268486302459767200L) :: field[W.`'community`.T](2) :: HNil,
        field[W.`'id`.T](4586302459767200L) :: field[W.`'community`.T](3) :: HNil
    ))
    val joined_dataset = join(
        graph_rt, rel_dataset,
        W('source) :: W('id) :: HNil,
        W('id) :: HNil,
        "inner",
        Model.JSON
    )
    println("\nSchema joined_dataset:")
    println(show(joined_dataset))
    println("\nJoined_dataset:")
    println(joined_dataset.data) */

    //println(show(Decompose[Relation[Field[W.`'id`.T, Long] :: Field[W.`'community`.T, Int] :: HNil]]))

    /* val dataset = JSON[CC_InputSchema1](dataset_cc)
    val test = add(dataset, W('retweeted_status) :: HNil, W('test), (x: dataset.Repr) => 0)
    println(show(test))
    println(test.data) */
    def get_community(node_id: Long): String = node_id match { case 1268486802949767200L => "C1" ; case 277430850L => "C1" ; case 1268486302459767200L => "C3" }

    val input_dataset1 = JSON[CC_InputSchema1](dataset_cc1)
    val input_dataset2 = JSON[CC_InputSchema2](dataset_cc2)
    val graph_rt = constructGraph(
        input_dataset1, 
        W('user) :: W('id) :: HNil, 
        W('retweeted_status) :: W('user) :: W('id) :: HNil, 
        //(W('user) :: W('name) :: HNil) :: HNil,
        //As(W('test), W('retweeted_status) :: W('user) :: W('name) :: HNil) :: HNil,
        HNil, HNil, HNil
    )
    val graph_rt_with_communities = transform(
        graph_rt,
        Add[String](W('source) :: HNil, W('community)) ::
        Add[String](W('dest) :: HNil, W('community)) :: HNil
    )(
        (dataset: graph_rt.type) => {
            dataset.data.map(hlist => {
                val source = get(hlist, W('source))
                val dest = get(hlist, W('dest))
                field[W.`'source`.T](source :+ field[W.`'community`.T](get_community(get(source, W('id))))) :: field[W.`'dest`.T](dest :+ field[W.`'community`.T](get_community(get(dest, W('id))))) :: field[W.`'edge`.T](get(hlist, W('edge))) :: HNil
            })
        }
    )
    val graph_rt_nodes = nodes(graph_rt_with_communities, Model.Relation)
    val graph_quotes = constructGraph(
        input_dataset2, 
        W('user) :: W('id) :: HNil, 
        W('quoted_status) :: W('user) :: W('id) :: HNil, 
        HNil, HNil, HNil
    )
    val joined_graph = join_in_right(
        graph_rt_nodes, graph_quotes,
        W('id) :: HNil,
        W('source) :: W('id) :: HNil,
        W('id), W('id)
    )
    val joined_graph2 = join_in_right(
        graph_rt_nodes, joined_graph,
        W('id) :: HNil,
        W('dest) :: W('id) :: HNil,
        W('id), W('id)
    )
    val adj_matrix = adjacency_matrix(joined_graph2, W('weight))
    println(adj_matrix)
    val comm_matrix = community_matrix(joined_graph2, W('community))
    println(comm_matrix)
    
    val workflow_output = adj_matrix
    show_dataset(workflow_output, "Workflow Output")
    println()
}