package pridwen

object Main extends App {  
    import shapeless.{HList, ::, HNil, Witness => W}
    import shapeless.labelled.{FieldType => Field, field}
    import pridwen.models._
    import pridwen.models.aux.{As}
    import pridwen.models.aux.transformations.{Add, Update}
    import pridwen.support.display._
    import pridwen.support.functions.{get}
    import pridwen.operators.predefined.construct._
    import pridwen.operators.predefined.join._
    import pridwen.operators.predefined.transform._
    import pridwen.operators.predefined.graph._
    
    //type InputSchema1 = Field[W.`'user`.T, Field[W.`'id`.T, Long] :: HNil] :: Field[W.`'retweeted_status`.T, Field[W.`'user`.T, Field[W.`'id`.T, Long] :: HNil] :: HNil] :: HNil


    // =============== Déclaration schéma(s) d'entrée

    case class User(id: Long, name: String)

    case class RetweetedStatus(user: User)
    case class QuotedStatus(user: User)

    case class TweetsRT(user: User, retweeted_status: RetweetedStatus)
    case class TweetsQuotes(user: User, quoted_status: QuotedStatus)



    // =============== Chargement des données

    val dataset_tweets_rt = List(
        TweetsRT(User(1268486802949767200L, "A"), RetweetedStatus(User(277430850L, "B"))),
        TweetsRT(User(1268486802949767200L, "A"), RetweetedStatus(User(277430850L, "B"))),
        TweetsRT(User(1268486302459767200L, "C"), RetweetedStatus(User(277430850L, "B"))),        
    )
    val dataset_tweets_quotes = List(
        TweetsQuotes(User(1268486802949767200L, "A"), QuotedStatus(User(277430850L, "B"))),
        TweetsQuotes(User(277430850L, "B"), QuotedStatus(User(1268486802949767200L, "A"))),
        TweetsQuotes(User(1268486302459767200L, "C"), QuotedStatus(User(1268486802949767200L, "A"))),        
    )

    val input_dataset1 = JSON[TweetsRT](dataset_tweets_rt)
    val input_dataset2 = JSON[TweetsQuotes](dataset_tweets_quotes)

    // Idée d'amélioration : que le schéma réel de dataset_tweets_rt ou dataset_tweets_quotes ne soit pas forcément strictement égal à TweetsRT ou TweetsQuotes mais inclus (= possède au moins les attributs spécifiés dans TweetsRT/TweetsQuotes)




    // =============== Fonctions auxiliaires
    
    def get_community(node_id: Long): String = node_id match { case 1268486802949767200L => "C1" ; case 277430850L => "C1" ; case 1268486302459767200L => "C3" }



    // =============== Construction du workflow

    // Step 1: Construction du graphe des retweets
    val graph_rt = constructGraph(
        input_dataset1, 
        W('user) :: W('id) :: HNil, 
        W('retweeted_status) :: W('user) :: W('id) :: HNil, 
        //(W('user) :: W('name) :: HNil) :: HNil,
        //As(W('test), W('retweeted_status) :: W('user) :: W('name) :: HNil) :: HNil,
        //HNil, HNil, HNil
    )

    // Step 2: Détection des communautés dans le graphe des retweets
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

    // Step 3: Récupération des sommets du graphe des retweets
    val graph_rt_nodes = nodes(graph_rt_with_communities, Model.Relation)

    // Step 4: Construction du graphe des quotes
    val graph_quotes = constructGraph(
        input_dataset2, 
        W('user) :: W('id) :: HNil, 
        W('quoted_status) :: W('user) :: W('id) :: HNil, 
        HNil, HNil, HNil
    )

    // Step 5: Jointure des deux graphes (seuls les sommets en commun sont conservés (inner), les sommets du graphe résultat récupèrent l'attribut de communauté)
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

    // Step 6: Construction de la matrice d'adjacence du graphe joint (matrice carrée d'entiers (poids) indicée par les ID des sommets)
    val adj_matrix = adjacency_matrix(joined_graph2, W('weight))

    // Step 7: Construction de la matrice des communautés du graphe joint (matrice de booléens indicée par les ID des sommets et par les valeurs distinctes des communautés)
    val comm_matrix = community_matrix(joined_graph2, W('community))

    // Step 8: Calcul de la polarisation
    val workflow_output = (
        (adj: adj_matrix.type, comm: comm_matrix.type) => { val nb_commu = comm.values.map(_.keys.toList).toList.flatten.distinct.length ; (List.fill(nb_commu){List.fill(nb_commu){0}}, List.fill(nb_commu){List.fill(nb_commu){0}}) }
    )(adj_matrix, comm_matrix)
    
    // Affichage des résultats
    show_dataset(workflow_output, "Workflow Output")
    println()



    // Idée d'amélioration : utiliser des DataSet en interne des différents types pour meilleure gestion des gros volumes de données.
}