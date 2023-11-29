import shapeless.{HList, ::, HNil, Witness => W}
import shapeless.labelled.{FieldType => Field, field}
import pridwen.models._
import pridwen.models.aux.{As}
import pridwen.support.display._
import pridwen.support.functions.{get}
import pridwen.operators.construct._
import pridwen.operators.join._
import pridwen.operators.transform._

import scala.collection.mutable.{ListBuffer, Map}

import time._

object Main extends App {  
    //data.toy_example

    // =============== Construction du workflow

    import data.{TweetsRT, TweetsRT2, TweetsQuotes, TweetsQuotes2}

    //data.clean_cs_data

    val csdata = true
    val (rep, file) = if(csdata) ("GMerged", "cs.json") else ("L2022-2", "")
    println("Input data")
    //val (input_dataset1, input_dataset2) = (JSON[TweetsRT](data.fake_tweets_rt), JSON[TweetsQuotes](data.fake_tweets_q)) 
    val (input_dataset1, input_dataset2) = time { val (data_rt, data_q) = data.load_json(rep, file) ; println("Création JSON") ; (time { JSON[TweetsRT2](data_rt) }, time { JSON[TweetsQuotes2](data_q) }) }

    // Step 1: Construction du graphe des retweets
    println("Construction graphe retweets")
    /* val graph_rt = time { constructGraph(
        input_dataset1, 
        W('user) :: W('id) :: HNil, 
        W('retweeted_status) :: W('user) :: W('id) :: HNil
    ).apply } */
    val graph_rt = time { construct_from(input_dataset1).aGraph(W('user) :: W('id) :: HNil, W('retweeted_status) :: W('user) :: W('id) :: HNil).construct }

    println(s"|V| = ${graph_rt.nodes.asList.size} ; |E| = ${graph_rt.data.size}")
    show_dataset(graph_rt, "Graph of Retweets")

    // Step 2: Détection des communautés dans le graphe des retweets
    println("Détection communautés")
    var graph_rt_with_communities = time { 
        if(!csdata) transform(graph_rt)((dataset: graph_rt.type) => community.detect_with_louvain(dataset, W('weight), W('community))).apply
        else transform(graph_rt)((dataset: graph_rt.type) => community.community_from_file(dataset)).apply
    }

    println(s"|V| = ${graph_rt.nodes.asList.size} ; |E| = ${graph_rt.data.size}")
    show_dataset(graph_rt_with_communities, "Graph of Retweets (with communities)")

    // Step 3: Suppression des sommets n'appartenant pas à une communauté significative
    println("Suppression communautés non-significatives")
    graph_rt_with_communities = time { community.keep_significant(graph_rt_with_communities, W('community)) }

    println(s"|V| = ${graph_rt_with_communities.nodes.asList.size} ; |E| = ${graph_rt_with_communities.data.size}")
    show_dataset(graph_rt_with_communities, "Graph of Retweets (only significant communities)")

    // Step 4: Récupération des sommets du graphe des retweets
    println("Récupération sommets retweets")
    val graph_rt_nodes = time { graph_rt_with_communities.nodes.as[Relation] }

    println(s"|rows| = ${graph_rt_nodes.data.size}")
    show_dataset(graph_rt_nodes, "Graph of Retweets Nodes")

    // Step 5: Construction du graphe des quotes
    println("Construction graphe citations")
    /* val graph_quotes = time { constructGraph(
        input_dataset2, 
        W('user) :: W('id) :: HNil, 
        W('quoted_status) :: W('user) :: W('id) :: HNil
    ).apply } */
    val graph_quotes = time { construct_from(input_dataset2).aGraph(W('user) :: W('id) :: HNil, W('quoted_status) :: W('user) :: W('id) :: HNil).construct }

    println(s"|V| = ${graph_quotes.nodes.asList.size} ; |E| = ${graph_quotes.data.size}")
    show_dataset(graph_quotes, "Graph of Quotes")

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

    println(s"|V| = ${joined_graph.nodes.asList.size} ; |E| = ${joined_graph.data.size}")
    show_dataset(joined_graph, "Joined Graph (Quote-RT)")


    // Step 7: Construction de la matrice d'adjacence du graphe joint (matrice carrée d'entiers (poids) indicée par les ID des sommets)
    /* val adj_matrix = time { joined_graph.adjacency_matrix(W('weight)) }

    //println(s"Size = ${adj_matrix.rows} x ${adj_matrix.cols}")
    show_dataset_nomodel(adj_matrix, "Adjacency Matrix", show_data = false)

    // Step 8: Construction de la matrice des communautés du graphe joint (matrice de booléens indicée par les ID des sommets et par les valeurs distinctes des communautés)
    val comm_matrix = time { joined_graph.community_matrix(W('community)) }

   // println(s"Size = ${adj_matrix.rows} x ${adj_matrix.cols}")
    show_dataset_nomodel(comm_matrix, "Community Matrix", show_data = false) */

    println("Création matrices adjacence et communauté")
    val (adj_matrix, comm_matrix) = time { polarisation.get_matrices(joined_graph, W('weight), W('community)) }

    println(s"${adj_matrix.rows}x${adj_matrix.cols}")
    println(s"${comm_matrix.rows}x${comm_matrix.cols}")

    // Step 9: Calcul de la polarisation
    println("Calcul polarisation")
    val workflow_output = time { polarisation.compute(adj_matrix, comm_matrix) }
    //val workflow_output = time { polarisation.compute(breeze.linalg.convert(breeze.linalg.csvread(new java.io.File("/home/alexis/Documents/Tweets/GMerged/ma.txt")), Int), breeze.linalg.convert(breeze.linalg.csvread(new java.io.File("/home/alexis/Documents/Tweets/GMerged/mc.txt")), Int)) }

    show_dataset_nomodel(workflow_output, "Workflow Output") 
    println()

    data.write_matrices[Double](Array((workflow_output._1, "ant"), (workflow_output._2, "por")))
}