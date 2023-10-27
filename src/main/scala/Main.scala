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

import scala.collection.mutable.{ListBuffer, Map}

import time._

object Main extends App {  

    // =============== Construction du workflow

    import data.{TweetsRT, TweetsRT2, TweetsQuotes, TweetsQuotes2}

    //data.clean_cs_data

    val csdata = true
    val (rep, file) = if(csdata) ("GMerged", "cs.json") else ("L2022-2", "")
    //val (input_dataset1, input_dataset2) = (JSON[TweetsRT](data.fake_tweets_rt), JSON[TweetsQuotes](data.fake_tweets_q)) 
    val (input_dataset1, input_dataset2) = { val (data_rt, data_q) = data.load_json(rep, file) ; (JSON[TweetsRT2](data_rt), JSON[TweetsQuotes2](data_q)) }

    // Step 1: Construction du graphe des retweets
    val graph_rt = time { constructGraph(
        input_dataset1, 
        W('user) :: W('id) :: HNil, 
        W('retweeted_status) :: W('user) :: W('id) :: HNil
    ) }

    println(s"|V| = ${graph_rt.nodes[Model.Relation].data.size} ; |E| = ${graph_rt.data.size}")
    show_dataset(graph_rt, "Graph of Retweets")

    // Step 2: Détection des communautés dans le graphe des retweets
    var graph_rt_with_communities = time { 
        if(!csdata) transform(graph_rt)((dataset: graph_rt.type) => community_detection.louvain(dataset, W('weight))) 
        else transform(graph_rt)((dataset: graph_rt.type) => community_detection.community_from_file(dataset))
    }

    println(s"|V| = ${graph_rt.nodes[Model.Relation].data.size} ; |E| = ${graph_rt.data.size}")
    show_dataset(graph_rt_with_communities, "Graph of Retweets (with communities)")

    // Step 3: Suppression des sommets n'appartenant pas à une communauté significative
    graph_rt_with_communities = time { community_detection.only_keep_significant2(graph_rt_with_communities, W('community)) }

    println(s"|V| = ${graph_rt_with_communities.nodes[Model.Relation].data.size} ; |E| = ${graph_rt_with_communities.data.size}")
    show_dataset(graph_rt_with_communities, "Graph of Retweets (only significant communities)")

    // Step 4: Récupération des sommets du graphe des retweets
    val graph_rt_nodes = time { graph_rt_with_communities.nodes[Model.Relation] }

    println(s"|rows| = ${graph_rt_nodes.data.size}")
    show_dataset(graph_rt_nodes, "Graph of Retweets Nodes")

    // Step 5: Construction du graphe des quotes
    val graph_quotes = time { constructGraph(
        input_dataset2, 
        W('user) :: W('id) :: HNil, 
        W('quoted_status) :: W('user) :: W('id) :: HNil
    ) }

    println(s"|V| = ${graph_quotes.nodes[Model.Relation].data.size} ; |E| = ${graph_quotes.data.size}")
    show_dataset(graph_quotes, "Graph of Quotes")

    // Step 6: Jointure des deux graphes (seuls les sommets en commun sont conservés (inner), les sommets du graphe résultat récupèrent l'attribut de communauté)
    val joined_graph = time { community_detection.only_keep_significant2(
                            join_in_right(graph_rt_nodes, 
                            join_in_right(graph_rt_nodes, graph_quotes,
                                W('id) :: HNil,
                                W('source) :: W('id) :: HNil
                            ),
                                W('id) :: HNil,
                                W('dest) :: W('id) :: HNil
                            ), W('community)) }

    println(s"|V| = ${joined_graph.nodes[Model.Relation].data.size} ; |E| = ${joined_graph.data.size}")
    show_dataset(joined_graph, "Joined Graph (Quote-RT)")


    // Step 7: Construction de la matrice d'adjacence du graphe joint (matrice carrée d'entiers (poids) indicée par les ID des sommets)
    val adj_matrix = time { joined_graph.adjacency_matrix(W('weight)) }

    //println(s"Size = ${adj_matrix.rows} x ${adj_matrix.cols}")
    show_dataset_nomodel(adj_matrix, "Adjacency Matrix", show_data = false)

    // Step 8: Construction de la matrice des communautés du graphe joint (matrice de booléens indicée par les ID des sommets et par les valeurs distinctes des communautés)
    val comm_matrix = time { joined_graph.community_matrix(W('community)) }

   // println(s"Size = ${adj_matrix.rows} x ${adj_matrix.cols}")
    show_dataset_nomodel(comm_matrix, "Community Matrix", show_data = false)

    //val (adj_matrix, comm_matrix) = time { polarisation.get_matrices(joined_graph, W('weight), W('community)) }

    //println(s"${adj_matrix.rows}x${adj_matrix.cols}")
    //println(s"${comm_matrix.rows}x${comm_matrix.cols}")

    //breeze.linalg.csvwrite(new java.io.File("/home/alexis/Documents/Tweets/GMerged/ma.txt"), breeze.linalg.convert(adj_matrix, Double))
    //breeze.linalg.csvwrite(new java.io.File("/home/alexis/Documents/Tweets/GMerged/mc.txt"), breeze.linalg.convert(comm_matrix, Double))

    // Step 9: Calcul de la polarisation
    /* val workflow_output = time { polarisation.compute(adj_matrix, comm_matrix) }
    //val workflow_output = time { polarisation.compute(breeze.linalg.convert(breeze.linalg.csvread(new java.io.File("/home/alexis/Documents/Tweets/GMerged/ma.txt")), Int), breeze.linalg.convert(breeze.linalg.csvread(new java.io.File("/home/alexis/Documents/Tweets/GMerged/mc.txt")), Int)) }

    show_dataset_nomodel(workflow_output, "Workflow Output") 
    println() */

    /* var m1 = "" ; var m2 = ""
    for(i <- 0 to (workflow_output._1.rows-1)) { for(j <- 0 to (workflow_output._1.cols-1)) { m1 += s"${workflow_output._1(i,j)} " ; m2 += s"${workflow_output._1(i,j)} " } ; m1 += "\n" ; m2 += "\n" }
    os.write.over(os.Path("/home/alexis/Documents/Tweets/GMerged/ant.txt"), m1)
    os.write.over(os.Path("/home/alexis/Documents/Tweets/GMerged/por.txt"), m2) */

    /* import scala.collection.mutable.HashMap
    val lm = HashMap(0 -> HashMap(0 -> 1, 1 -> 3, 2 -> 2), 1 -> HashMap(0 -> 1), 2 -> HashMap(0 -> 1, 1 -> 2, 2 -> 2))
    val rm = HashMap(0 -> HashMap(2 -> 2), 1 -> HashMap(0 -> 7, 1 -> 5), 2 -> HashMap(0 -> 2, 1 -> 1, 2 -> 1))
    println(polarisation.elwise_product(lm, rm))
    println(polarisation.product(lm, rm))
    println()

    println(polarisation.compute(polarisation.adj_toyex, polarisation.comm_toyex)) */
}