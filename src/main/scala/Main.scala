import pridwen.models.{JSON, Relation}
import pridwen.operators.display.{show_dataset, show_dataset_nomodel}
import pridwen.operators.construct.construct_from
import pridwen.operators.join.join_in_right
import pridwen.operators.transform.transform

import shapeless.{::, HNil, Witness => W}

import time._

object Main extends App {  
    // =============== Chargement des données

    case class User(id: Double)
    case class RetweetedStatus(user: User) ; case class QuotedStatus(user: User)
    case class TweetsRT(user: User, retweeted_status: RetweetedStatus) ; case class TweetsQuotes(user: User, quoted_status: QuotedStatus)

    println("Input data")
    val (input_dataset1, input_dataset2) = time { val (data_rt, data_q) = data.load_json("GMerged", "cs.json") ; println("Création JSON") ; (time { JSON[TweetsRT](data_rt) }, time { JSON[TweetsQuotes](data_q) }) }

    // =============== Construction du workflow

    // Step 1: Construction du graphe des retweets
    println("Construction graphe retweets")
    val graph_rt = time { construct_from(input_dataset1).aGraph(W('user) :: W('id) :: HNil, W('retweeted_status) :: W('user) :: W('id) :: HNil).construct }

    show_dataset(graph_rt, "Graph of Retweets")
    println(s"|V| = ${graph_rt.nodes.asList.size} ; |E| = ${graph_rt.data.size}")

    // Step 2: Détection des communautés dans le graphe des retweets
    println("Détection communautés")
    var graph_rt_with_communities = time { 
        //transform(graph_rt)((dataset: graph_rt.type) => community.detect_with_louvain(dataset, W('weight), W('community))).apply
        transform(graph_rt)((dataset: graph_rt.type) => community.community_from_file(dataset)).apply
    }

    show_dataset(graph_rt_with_communities, "Graph of Retweets (with communities)")
    println(s"|V| = ${graph_rt.nodes.asList.size} ; |E| = ${graph_rt.data.size}")

    // Step 3: Suppression des sommets n'appartenant pas à une communauté significative
    println("Suppression communautés non-significatives")
    graph_rt_with_communities = time { community.keep_significant(graph_rt_with_communities, W('community)) }

    show_dataset(graph_rt_with_communities, "Graph of Retweets (only significant communities)")
    println(s"|V| = ${graph_rt_with_communities.nodes.asList.size} ; |E| = ${graph_rt_with_communities.data.size}")

    // Step 4: Récupération des sommets du graphe des retweets
    println("Récupération sommets retweets")
    val graph_rt_nodes = time { graph_rt_with_communities.nodes.as[Relation] }

    show_dataset(graph_rt_nodes, "Graph of Retweets Nodes")
    println(s"|rows| = ${graph_rt_nodes.data.size}")

    // Step 5: Construction du graphe des quotes
    println("Construction graphe citations")
    val graph_quotes = time { construct_from(input_dataset2).aGraph(W('user) :: W('id) :: HNil, W('quoted_status) :: W('user) :: W('id) :: HNil).construct }

    show_dataset(graph_quotes, "Graph of Quotes")
    println(s"|V| = ${graph_quotes.nodes.asList.size} ; |E| = ${graph_quotes.data.size}")

    // Step 6: Jointure des deux graphes (seuls les sommets en commun sont conservés (inner), les sommets du graphe résultat récupèrent l'attribut de communauté)
    println("Jointures")
    val joined_graph = time { community.keep_significant(
                            time { join_in_right(graph_rt_nodes, 
                            time { join_in_right(graph_rt_nodes, graph_quotes,
                                W('id) :: HNil,
                                W('source) :: W('id) :: HNil
                            )},
                                W('id) :: HNil,
                                W('dest) :: W('id) :: HNil
                            )}, W('community)) }

    show_dataset(joined_graph, "Joined Graph (Quote-RT)")
    println(s"|V| = ${joined_graph.nodes.asList.size} ; |E| = ${joined_graph.data.size}")

    // Step 7: Création des matrices d'adjacence et de communauté (entrées algo polarisation)
    println("Création matrices adjacence et communauté")
    val (adj_matrix, comm_matrix) = time { polarisation.get_matrices(joined_graph, W('weight), W('community)) }

    println(s"Ma: ${adj_matrix.rows}x${adj_matrix.cols} - Mc: ${comm_matrix.rows}x${comm_matrix.cols}")

    // Step 9: Calcul de la polarisation
    println("Calcul polarisation")
    val workflow_output = time { polarisation.compute(adj_matrix, comm_matrix) }

    show_dataset_nomodel(workflow_output, "Workflow Output") 
    println()

    data.write_matrices[Double](Array((workflow_output._1, "ant"), (workflow_output._2, "por")))
}