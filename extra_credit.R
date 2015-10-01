source("graph.R")

#minimum spanning tree
min_span_tree <- function(g){
  if(!is_valid(g)){
    stop("Please input a valid graph.")
  }
  
  if(!is_undirected(g)){
    stop("Please input an undirected graph")
  }
  
  edges <- edge(g)
  #Remove redundancies for the edge table of an Undirected graph
  edges <- edges[edges$start < edges$end,]
  #The following code will apply Prim's Algorithm
  edges <- edges[order(edges$weight,edges$start,edges$end),]
  MST <- edges[1,]
  vertices <- c(MST$start,MST$end) #record all the vertices in MST
  while(nrow(MST) < length(g) - 1){
    neighbor <- edges[(edges$start %in% vertices | edges$end %in% vertices) & (!(edges$start %in% vertices) | !(edges$end %in% vertices)),]
    #neighbor <- diff_df(neigbor,MST)
    #protection for non-connected components
    if(nrow(neighbor) == 0){
      stop("The input graph is not connected.")
    }
    MST <- rbind(MST,setNames(as.list(neighbor[1,]), names(MST)))
    vertices <- c(vertices,MST$start,MST$end)
  }
  return(edgeToList(edgeToUndirected(MST)))
}
edgeToUndirected <- function(MST){
  MST2 <- MST
  for(i in 1:nrow(MST2)){
    tmp <- data.frame(start = MST2[i,]$end,end =MST2[i,]$start, weight = MST2[i,]$weight)
    MST <- rbind(MST,tmp)
  }
  return(MST)
}
edgeToList <- function(MST){
  vertices <- sort(unique(c(MST$start, MST$end)))
  MST <- MST[order(MST$start,MST$end),]
  orders <- order(vertices)
  table <- as.data.frame(cbind(vertices,orders),stringsAsFactors = FALSE)
  res <- list()
  for(vert in vertices){
    element <- list(edges = as.integer(table[table$vertices %in% MST[MST$start == vert,]$end,]$order),weights = MST[MST$start == vert,]$weight)
    res <- append(res,list(vert = element))
    names(res)[length(res)] <- vert
  }
  return(res)
}

