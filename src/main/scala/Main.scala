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

    // Step 0
    println("-- Loading input data (step 0/8)")
    val (input_dataset_rt, input_dataset_q) = time { 
        println("--- Loading the file (substep 1/2)")
        val (data_rt, data_q) = time { data.load_json("GMerged", "cs.json") } 
        println("--- Modelling data in JSON (substep 2/2)")
        (time { JSON[TweetsRT](data_rt) }, time { JSON[TweetsQuotes](data_q) }) 
    }

    // =============== Construction du workflow

    // Step 1
    println("-- Building the graph of retweets (step 1/8)")
    val graph_rt = time { construct_from(input_dataset_rt).aGraph(W('user) :: W('id) :: HNil, W('retweeted_status) :: W('user) :: W('id) :: HNil).construct }

    show_dataset(graph_rt, "Graph of Retweets")
    println(s"|V| = ${graph_rt.nodes.asList.size} ; |E| = ${graph_rt.data.size}\n")

    // Step 2
    println("-- Community detection (step 2/8)")
    var graph_rt_with_communities = time { 
        //transform(graph_rt)((dataset: graph_rt.type) => community.detect_with_louvain(dataset, W('weight), W('community))).apply
        transform(graph_rt)((dataset: graph_rt.type) => community.community_from_file(dataset)).apply
    }

    show_dataset(graph_rt_with_communities, "Graph of Retweets (with communities)")
    println(s"|V| = ${graph_rt_with_communities.nodes.asList.size} ; |E| = ${graph_rt_with_communities.data.size}\n")

    // Step 3
    println("-- Elimination of non-significant communities (step 3/8)")
    graph_rt_with_communities = time { community.keep_significant(graph_rt_with_communities, W('community)) }

    show_dataset(graph_rt_with_communities, "Graph of Retweets (only significant communities)")
    println(s"|V| = ${graph_rt_with_communities.nodes.asList.size} ; |E| = ${graph_rt_with_communities.data.size}\n")

    // Step 4
    println("-- Extracting the nodes of the graph of retweets (step 4/8)")
    val graph_rt_nodes = time { graph_rt_with_communities.nodes.as[Relation] }

    show_dataset(graph_rt_nodes, "Graph of Retweets nodes")
    println(s"|rows| = ${graph_rt_nodes.data.size}\n")

    // Step 5
    println("-- Building the graph of quotes (step 5/8)")
    val graph_quotes = time { construct_from(input_dataset_q).aGraph(W('user) :: W('id) :: HNil, W('quoted_status) :: W('user) :: W('id) :: HNil).construct }

    show_dataset(graph_quotes, "Graph of Quotes")
    println(s"|V| = ${graph_quotes.nodes.asList.size} ; |E| = ${graph_quotes.data.size}\n")

    // Step 6
    println("-- Joining the graph of quotes with the nodes of the graph of retweets (step 6/8)")
    val joined_graph = time { community.keep_significant(time { 
        join_in_right(
            graph_rt_nodes, 
            time { join_in_right(graph_rt_nodes, graph_quotes, W('id) :: HNil, W('source) :: W('id) :: HNil) },
            W('id) :: HNil, W('dest) :: W('id) :: HNil
        )
    }, W('community)) }

    show_dataset(joined_graph, "Joined Graph (Quote-RT)")
    println(s"|V| = ${joined_graph.nodes.asList.size} ; |E| = ${joined_graph.data.size}\n")

    // Step 7
    println("-- Creating the adjacency and community matrices (step 7/8)")
    val (adj_matrix, comm_matrix) = time { polarisation.get_matrices(joined_graph, W('weight), W('community)) }

    println(s"Adjacency matrix: ${adj_matrix.rows}x${adj_matrix.cols} - Community matrix: ${comm_matrix.rows}x${comm_matrix.cols}\n")

    // Step 8
    println("-- Calculating polarisation measurements (step 8/8)")
    val workflow_output = time { polarisation.compute(adj_matrix, comm_matrix) }

    show_dataset_nomodel(workflow_output, "Workflow Output") 
    data.write_matrices[Double](Array((workflow_output._1, "ant"), (workflow_output._2, "por")))
    println()
}
