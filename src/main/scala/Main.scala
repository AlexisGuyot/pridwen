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

object Main extends App {  
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
        TweetsQuotes(User(1268486802949767200L, "A"), QuotedStatus(User(1268486302459767200L, "C"))),
        TweetsQuotes(User(277430850L, "B"), QuotedStatus(User(1268486802949767200L, "A"))),
        TweetsQuotes(User(1268486302459767200L, "C"), QuotedStatus(User(1268486802949767200L, "A"))),    
        TweetsQuotes(User(2, "D"), QuotedStatus(User(1268486802949767200L, "A"))), 
        TweetsQuotes(User(2, "D"), QuotedStatus(User(3, "E"))),         
        TweetsQuotes(User(2, "D"), QuotedStatus(User(3, "E"))),       
    )

    val input_dataset1 = JSON[TweetsRT](dataset_tweets_rt)
    val input_dataset2 = JSON[TweetsQuotes](dataset_tweets_quotes)

    // Idée d'amélioration : que le schéma réel de dataset_tweets_rt ou dataset_tweets_quotes ne soit pas forcément strictement égal à TweetsRT ou TweetsQuotes mais inclus (= possède au moins les attributs spécifiés dans TweetsRT/TweetsQuotes)
    // Suite : charger de vraies données




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
    val graph_rt_with_communities = transform(graph_rt)(
        (dataset: graph_rt.type) => {
            dataset.data.map(hlist => {
                val source = get(hlist, W('source))
                val dest = get(hlist, W('dest))
                field[W.`'source`.T](source :+ field[W.`'community`.T](get_community(get(source, W('id))))) :: field[W.`'dest`.T](dest :+ field[W.`'community`.T](get_community(get(dest, W('id))))) :: field[W.`'edge`.T](get(hlist, W('edge))) :: HNil
            })
        }
    )

    show_dataset(graph_rt_with_communities, "Graph of Retweets (with communities)")

    // Step 3: Récupération des sommets du graphe des retweets
    val graph_rt_nodes = graph_rt_with_communities.nodes[Model.Relation]

    show_dataset(graph_rt_nodes, "Graph of Retweets Nodes")

    // Step 4: Construction du graphe des quotes
    val graph_quotes = constructGraph(
        input_dataset2, 
        W('user) :: W('id) :: HNil, 
        W('quoted_status) :: W('user) :: W('id) :: HNil, 
        HNil, HNil, HNil
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

    show_dataset_nomodel(adj_matrix, "Adjacency Matrix")

    // Step 7: Construction de la matrice des communautés du graphe joint (matrice de booléens indicée par les ID des sommets et par les valeurs distinctes des communautés)
    val comm_matrix = joined_graph2.community_matrix(W('community))

    show_dataset_nomodel(comm_matrix, "Community Matrix")

    // Step 8: Calcul de la polarisation
    val workflow_output = (
        (adj: adj_matrix.type, comm: comm_matrix.type) => { val nb_commu = comm.values.map(_.keys.toList).toList.flatten.distinct.length ; (List.fill(nb_commu){List.fill(nb_commu){0}}, List.fill(nb_commu){List.fill(nb_commu){0}}) }
    )(adj_matrix, comm_matrix)

    show_dataset_nomodel(workflow_output, "Workflow Output")
    println()

    import polarisation._
    import scala.collection.mutable.Map
    val adj: Map[Int, Map[Int, Int]] = Map(1 -> Map(2 -> 3, 3 -> 2, 4 -> 4, 6 -> 5), 2 -> Map(3 -> 1, 6 -> 6), 3 -> Map(2 -> 5), 4 -> Map(1 -> 1, 5 -> 5, 6 -> 4), 6 -> Map(7 -> 5))
    val comm: Map[Int, Map[String, Boolean]] = Map(1 -> Map("C1" -> true), 2 -> Map("C1" -> true), 3 -> Map("C1" -> true), 4 -> Map("C2" -> true), 5 -> Map("C2" -> true), 6 -> Map("C2" -> true, "C3" -> true), 7 -> Map("C3" -> true))
    val (polarisation_matrix, porosity_matrix) = polarisation(adj, comm)
    println(polarisation_matrix)
    println(porosity_matrix)

    // Idée d'amélioration : utiliser des DataSet en interne des différents types pour meilleure gestion des gros volumes de données.
}