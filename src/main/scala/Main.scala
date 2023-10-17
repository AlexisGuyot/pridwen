import shapeless.{HList, ::, HNil, Witness => W}
import shapeless.labelled.{FieldType => Field, field}
import pridwen.models._
import pridwen.models.aux.{As}
import pridwen.models.aux.transformations.{Add, Update}
import pridwen.support.display._
import pridwen.support.functions.{get}
import pridwen.operators.construct._
import pridwen.operators.join._
import pridwen.operators.transform._

import scala.collection.mutable.ListBuffer

object Main extends App {  

    // =============== Chargement des données (fictives)

    case class NUser(id: Long, name: String)

    case class RetweetedStatus(user: NUser)
    case class QuotedStatus(user: NUser)

    case class TweetsRT(user: NUser, retweeted_status: RetweetedStatus)
    case class TweetsQuotes(user: NUser, quoted_status: QuotedStatus)

    val dataset_tweets_rt = List(
        TweetsRT(NUser(1268486802949767200L, "A"), RetweetedStatus(NUser(277430850L, "B"))),
        TweetsRT(NUser(1268486802949767200L, "A"), RetweetedStatus(NUser(277430850L, "B"))),
        TweetsRT(NUser(1268486302459767200L, "C"), RetweetedStatus(NUser(277430850L, "B"))),        
    )
    val dataset_tweets_quotes = List(
        TweetsQuotes(NUser(1268486802949767200L, "A"), QuotedStatus(NUser(277430850L, "B"))),
        TweetsQuotes(NUser(1268486802949767200L, "A"), QuotedStatus(NUser(1268486302459767200L, "C"))),
        TweetsQuotes(NUser(277430850L, "B"), QuotedStatus(NUser(1268486802949767200L, "A"))),
        TweetsQuotes(NUser(1268486302459767200L, "C"), QuotedStatus(NUser(1268486802949767200L, "A"))),    
        TweetsQuotes(NUser(2, "D"), QuotedStatus(NUser(1268486802949767200L, "A"))), 
        TweetsQuotes(NUser(2, "D"), QuotedStatus(NUser(3, "E"))),         
        TweetsQuotes(NUser(2, "D"), QuotedStatus(NUser(3, "E"))),       
    )

    val input_dataset1 = JSON[TweetsRT](dataset_tweets_rt)
    val input_dataset2 = JSON[TweetsQuotes](dataset_tweets_quotes)

    // Idée d'amélioration : que le schéma réel de dataset_tweets_rt ou dataset_tweets_quotes ne soit pas forcément strictement égal à TweetsRT ou TweetsQuotes mais inclus (= possède au moins les attributs spécifiés dans TweetsRT/TweetsQuotes)


    // =============== Chargement des données (réelles)

    def rec_contains(json: ujson.Value, path: List[String]): Boolean = if(path.isEmpty) true else json.obj.contains(path.head) && rec_contains(json(path.head), path.tail)
    def rec_format(json: ujson.Value, path: List[String]): String = if(path.length == 1) s""""${path.head}": ${json(path.head)}""" else s""""${path.head}": { ${rec_format(json(path.head), path.tail)} }"""
    def select_fields(json: ujson.Value, paths: List[List[String]], acc: List[String] = List()): List[String] = if(paths.isEmpty) acc else select_fields(json, paths.tail, if(rec_contains(json, paths.head)) rec_format(json, paths.head)::acc else acc)


    val path = os.Path("/home/alexis/Documents/Tweets")
    /* var fields = ListBuffer[String]()
    for(file <- os.list(path / "2022-2")) for(line <- os.read.lines.stream(file)) {
        val json = ujson.read(line)
        fields = select_fields(json, List(List("id"), List("text"), List("user", "id"))).to(ListBuffer)
        if(rec_contains(json, List("retweeted_status"))) fields += """"retweeted_status": {""" + select_fields(json("retweeted_status"), List(List("id"), List("text"), List("user", "id"))).mkString(", ") + " }"
        if(rec_contains(json, List("quoted_status"))) fields += """"quoted_status": {""" + select_fields(json("quoted_status"), List(List("id"), List("text"), List("user", "id"))).mkString(", ") + " }"
        os.write.append(path / "L2022-2" / (file.last + ".json"), "{ " + fields.mkString(", ") + " }\n")
    } */
 
    case class User(id: Long)

    case class RetweetedStatus2(user: User)
    case class QuotedStatus2(user: User)

    case class TweetsRT2(user: User, retweeted_status: RetweetedStatus2) 
    case class TweetsQuotes2(user: User, quoted_status: QuotedStatus2)

    var dataset_tweets_rt2 = ListBuffer[TweetsRT2]()
    var dataset_tweets_quotes2 = ListBuffer[TweetsQuotes2]()
    var cmp = 0
    val nb_files = os.list(path / "L2022-2").size
    os.list(path / "L2022-2").foreach(file => { os.read.lines.stream(file).foreach(line => {
        val json = ujson.read(line)
        if(json.obj.contains("retweeted_status")) dataset_tweets_rt2 += TweetsRT2(User(json("user")("id").num.longValue), RetweetedStatus2(User(json("retweeted_status")("user")("id").num.longValue)))
        if(json.obj.contains("quoted_status")) dataset_tweets_quotes2 += TweetsQuotes2(User(json("user")("id").num.longValue), QuotedStatus2(User(json("quoted_status")("user")("id").num.longValue)))
    }) ; println(s"${cmp+1}/${nb_files}: dataset_tweets_rt2 = ${dataset_tweets_rt2.size} ; dataset_tweets_quotes2 = ${dataset_tweets_quotes2.size}") ; cmp += 1 })
    println(dataset_tweets_rt2.size)
    println(dataset_tweets_quotes2.size)
    val input_dataset12 = JSON[TweetsRT2](dataset_tweets_rt2.toList)
    val input_dataset22 = JSON[TweetsQuotes2](dataset_tweets_quotes2.toList)


    // =============== Fonctions auxiliaires
    
    



    // =============== Construction du workflow

    // Step 1: Construction du graphe des retweets
    val graph_rt = constructGraph(
        input_dataset1, 
        //input_dataset12,
        W('user) :: W('id) :: HNil, 
        W('retweeted_status) :: W('user) :: W('id) :: HNil, 
        //(W('user) :: W('name) :: HNil) :: HNil,
        //As(W('test), W('retweeted_status) :: W('user) :: W('name) :: HNil) :: HNil,
        //HNil, HNil, HNil
    )

    show_dataset(graph_rt, "Graph of Retweets")

    // Step 2: Détection des communautés dans le graphe des retweets
    /* val graph_rt_with_communities = transform(
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
    ) */
    val graph_rt_with_communities = transform(graph_rt)((dataset: graph_rt.type) => community_detection.louvain(dataset, W('weight)))

    show_dataset(graph_rt_with_communities, "Graph of Retweets (with communities)")

    // Step 3: Récupération des sommets du graphe des retweets
    val graph_rt_nodes = graph_rt_with_communities.nodes[Model.Relation]

    show_dataset(graph_rt_nodes, "Graph of Retweets Nodes")

    // Step 4: Construction du graphe des quotes
    val graph_quotes = constructGraph(
        input_dataset2, 
        //input_dataset22,
        W('user) :: W('id) :: HNil, 
        W('quoted_status) :: W('user) :: W('id) :: HNil
    )

    show_dataset(graph_quotes, "Graph of Quotes")

    // Step 5: Jointure des deux graphes (seuls les sommets en commun sont conservés (inner), les sommets du graphe résultat récupèrent l'attribut de communauté)
    val joined_graph = join_in_right(
        graph_rt_nodes, graph_quotes,
        W('id) :: HNil,
        W('source) :: W('id) :: HNil
    )
    val joined_graph2 = join_in_right(
        graph_rt_nodes, joined_graph,
        W('id) :: HNil,
        W('dest) :: W('id) :: HNil
    )

    show_dataset(joined_graph2, "Joined Graph (Quote-RT)")

    // Step 6: Construction de la matrice d'adjacence du graphe joint (matrice carrée d'entiers (poids) indicée par les ID des sommets)
    val adj_matrix = joined_graph2.adjacency_matrix(W('weight))

    show_dataset_nomodel(adj_matrix, "Adjacency Matrix", show_data = false)

    // Step 7: Construction de la matrice des communautés du graphe joint (matrice de booléens indicée par les ID des sommets et par les valeurs distinctes des communautés)
    val comm_matrix = joined_graph2.community_matrix(W('community))

    show_dataset_nomodel(comm_matrix, "Community Matrix", show_data = false)

    // Step 8: Calcul de la polarisation
    /* val workflow_output = (
        (adj: adj_matrix.type, comm: comm_matrix.type) => { val nb_commu = comm.values.map(_.keys.toList).toList.flatten.distinct.length ; (List.fill(nb_commu){List.fill(nb_commu){0}}, List.fill(nb_commu){List.fill(nb_commu){0}}) }
    )(adj_matrix, comm_matrix) */
    val workflow_output = (
        (adj: adj_matrix.type, comm: comm_matrix.type) => polarisation.compute(adj, comm)
    )(adj_matrix, comm_matrix)

    show_dataset_nomodel(workflow_output, "Workflow Output")
    println()
    
    println(polarisation.compute(polarisation.adj_toyex, polarisation.comm_toyex))

    // Idée d'amélioration : utiliser des DataSet en interne des différents types pour meilleure gestion des gros volumes de données.
}