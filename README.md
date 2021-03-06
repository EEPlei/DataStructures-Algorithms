[![wercker status](https://app.wercker.com/status/059f4fa3743e3390d515ccb3cbd10c0a/m "wercker status")](https://app.wercker.com/project/bykey/059f4fa3743e3390d515ccb3cbd10c0a)

# Team4_hw1   


## Background

For this homework we are going to explore the world of data structures and algorithms. This is a huge subject and we will only be able to examine a tiny subset of a tiny subset of this area. Specifically, we will focus on the mathematical concept of a graph - a collection of nodes (vertices) some subset of which are connected to one another by edges. For this homework we will specify a simple data structure in R to represent these graphs and you will be responsible for implementing a number of common and useful algorithms for working with this kind of data.

<br/>

### Graphs

* A graph G = (V,E) is simply a set of vertices V, and a set of edges E between those vertices. It may be more convenient to think about a graph as being a network.

* Edges in graphs can be directed or undirected, the difference being whether the relationship is mutual or one-sided.

* Edges can be weighted, the value of the weight represents some quantitative measure. Most often these weights are thought of as being the distance between the connected vertices.

* A path is a sequence of edges that connects two vertices. There may be many paths, or no paths, between two vertices in a graph.

<br/>

### Data Structure

We will simplify things somewhat by using only a single data structure for the most general case - a labeled and weighted directed graph. We will represent this as a list of lists in R. The primary list will consist of the graphs vertices which are identified by a *unique* label. These labels should be stored in the primary list's names attribute - a graph object without these labels is considered invalid.

The secondary lists stored in primary list contain the properties of each vertex, specifically an integer vector named `edges` which stores the indices of vertices connected to the current vertex. Since our data structure is for a directed graph, we will assume that all of these connects are from the current vertex to the listed vertices. It is allowed to have edges that connect a vertex back to itself. The second element for each secondary list is a numeric vector named `weights` that contains the weights of each connection listed in the `edges` vector. We again assume that all edges are weighted, if a weight is not specified it should be assumed to be 1 and that all weights should be strictly greater than 0.

Below are example representations of the two graphs at the top of this document:

```{r}
graph1 = list(A = list(edges   = c(2L),
                       weights = c(1 )),
              B = list(edges   = c(3L),
                       weights = c(1 )),
              C = list(edges   = c(5L),
                       weights = c(1 )),
              D = list(edges   = c(2L),
                       weights = c(1 )),
              E = list(edges   = c(4L,6L),
                       weights = c(1,1  )),
              F = list(edges   = c(),
                       weights = c())
             )
str(graph1)
```

```{r}
graph2 = list(A = list(edges   = c(2L),
                       weights = c(14)),
              B = list(edges   = c(3L,4L),
                       weights = c(23,13)),
              D = list(edges   = c(1L),
                       weights = c(5) ),
              F = list(edges   = c(1L,5L),
                       weights = c(43,33)),
              N = list(edges   = c(1L,2L,4L),
                       weights = c(33,22,11))
             )
str(graph2)
```

### Requirements

## Functions

Below are a list of functions that you will need to implement for this assignment. For each function you will be given a name, the input arguments, the output type, and a description of the required behavior of the function. Your implementation of these functions may only use R's base functionality, meaning you are not allowed to directly use any additional packages in your implementation. However, you may use other packages for testing purposes.

Be aware that there are we established algortihms to solve most of the problems listed below - don't try to reinvent the wheel use what is known to work. Your task is translate the existing algorithms to our specific data structure and R.

* Function - `is_valid`

    * Input - `g`, a graph object.

    * Output - `TRUE` if `g` is valid, `FALSE` if not.

    * Description - Validate the graph object to ensure that it meets all requirements - Check that object is a list of lists. Check if there are names for the primary list that they are all unique. Check that each secondary list contains only edges and weights vectors that are of the appropriate type. Check that there are not any edges to non-existent vertices. Check that all weights are not less than or equal to 0. Check that every edge has a weight.

* Function - `is_undirected`

    * Input - `g`, a graph object.

    * Output - `TRUE` if `g` is undirected, `FALSE` if not.

    * Description - Check if the graph object is undirected, this is true if all directed edges have a complementary directed edge with the same weight in the opposite direction.

* Function - `is_isomorphic`

    * Input - `g1`, a graph object; `g2`, a graph object.

    * Output - `TRUE` if `g1` and `g2` are isomorphic, `FALSE` if not.

    * Description - Check if the graph objects are isomorphic, meaning all vertices, edges, and weights are identical.
    Comparison of vertices should be based on names not indexes.

* Function - `is_connected`

    * Input - `g`, a graph object; `v1`, a vertex label in `g`; `v2`, a vertex label in `g`.

    * Output - `TRUE` if there is a path from `v1` to `v2` in g, `FALSE` if not.

    * Description - Determine if there is any path between vertex `v1` and vertex `v2` in graph `g`. If `v1` or `v2` are not in g then throw an error.

* Function - `shortest_path`

    * Input - `g`, graph object; `v1`, a vertex label in `g`; `v2`, a vertex label in `g`.

    * Output - a vector of the names of vertices that make up the shortest path, in order. If there is no path between the vertices then return an empty vector.

    * Description - Find the shortest path from vertex `v1` to vertex `v2` using the edges of graph g. Note that there may not be a unique solution for any given graph, you are only required to return one path.


### Extra Credit

* Function - `min_span_tree`

    * Input - `g`, a graph object.

    * Output - a graph object (undirected) containing the minimum spanning tree

    * Description - A tree is an undirected graph in which any two vertices are connected by exactly one path (no simple cycles). Therefore, a minimum spanning tree is the tree that connects all vertices in a graph with the shortest possible total of edges, using the existing edges. If given a directed graph return an error. Note that there may not be a unique solution for any given graph, you are only required to return one tree.

* Function - `plot_graph`

    * Input - `g`, a graph object

    * Output - an R plot showing all vertices (labeled) and edges. 

    * Description - This function should be able to take any graph object and produce a reasonably attractive visual representation of that graph. Your algorithm should make use edge weights to layout the distance between vertices.

<br/>

## Testing

As this and next week progresses I will be adding tests (using the `testthat` package) for these functions to every team's repository. We will be using the wercker continuous integration tool, so that you will be given real time feedback on your progress via github. This process and these tools will be discussed in more detail in class in the coming weeks.

## Work Product

For this assignment you will need to create the following files:

* `graph.R` - this file should implement the five functions detailed above. Each function must be named exactly as described and accept arguments exactly as described.

* `hw1.Rmd` - this document detail how your group has chosen to implement each of the five functions - you should be sure to cite any external resources you use and specifically mention any existing algorithms that your approach is based on. Each function should be given its own section and your writeup should also demonstrate the functionality of your implementations (i.e. include sample code using your function). This file should not include any of your implementations, those should be loaded from `graph.R` using `source(graph.R)` to make the functions available within the document.

* `extra_credit.R` - if you attempt either of the extra credit functions these should be included in this separate file. Tests will be provided for the `min_span_tree` function but not `plot_graph`.




<div style="text-align: left">
Assignment from STA 523 Statistical Programming with Professor Colin Rundel    
</div>   
<div style="text-align: left">
http://www2.stat.duke.edu/~cr173/Sta523_Fa15/hw/hw0.html 
</div>

