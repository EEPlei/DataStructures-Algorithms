#is_valid function
#check if the graph is a list
#check if the names for every element are all unique
#check if every element is a list
#check if every element only contains edges and weights that are of the appropriate type
#check there are any edges to non-existent vertices
#check if all weights are not less than or equal to 0
#check if every edge has a weight
is_valid <- function(g){
  if(typeof(g)!="list")
    return(FALSE)
  if(length(g) != length(unique(names(g))))
    return(FALSE) 
  for(i in 1:length(g)){
    if(typeof(g[[i]]) != "list")
      return(FALSE)
    if(!("edges" %in% names(g[[i]]) & "weights" %in% names(g[[i]]))) 
      +     return(FALSE)
    if(length(g[[i]]$edges) | length(g[[i]]$weights) > 0)
      if(any(is.na(g[[i]]$edges)) | any(is.na(g[[i]]$weights)))
        return(FALSE)
    if(typeof(g[[i]]$edges) != "integer" & typeof(g[[i]]$edges) != "NULL")
      return(FALSE)
    if(class(g[[i]]$weights) != "numeric" & class(g[[i]]$weights) != "NULL")
      return(FALSE)
    if(any(g[[i]]$edges>length(g)) | any(g[[i]]$edges<1))
      return(FALSE)
    if(any(g[[i]]$weights <= 0))
      return(FALSE)
    if(length(g[[i]]$edges) != length(g[[i]]$weights))
      return(FALSE)
    if(length(g[[i]]$edges) != length(unique(g[[i]]$edges)))
      return(FALSE)
  }
  return(TRUE)
} 

#is_undirected function
#to check whether its edge data frame is symmetric
is_undirected <- function(g){
  
  if(!is_valid(g)){ #if graph is not valid, return an error
    stop("Please input a valid graph")
  }
  
  edges <- edge(g)
  if (nrow(edges)>0) #again the loop runs when the array starts from 0
    for(i in 1:nrow(edges)){
      istart <- edges[i,]$start
      iend <- edges[i,]$end
      iweight <- edges[i,]$weight
      if(nrow(edges[edges$start == iend & edges$end == istart,]) == 0)
        return(FALSE)
      # if there are no rows where the starting vertex #
      # is the same as the ending vertex for our ith row #
      # and have the same ending vertex # 
      # as our ith row's starting vertex #
      # return FALSE #
      if(edges[edges$start == iend & edges$end == istart,]$weight != iweight)
        return(FALSE)
      # if that row does exist, if the weight of that row #
      # isn't the same weight as our ith row #
      # return FALSE #
    } 
  return(TRUE)
}

edge <- function(g){
  # function where input is 'g' #
  output <- data.frame(start = numeric(), end = numeric(), weight = numeric(),stringsAsFactors =F)
  # makes an empty dataframe. where the horizontal header has "start", "end", and "weight" # 
  # numeric() creates a zero vector for each column #
  for(i in 1:length(g)){
    cur <- g[i]
    curvec <- unlist(cur)
    num_to <- length(curvec)/2 
    #number of vertices the current vertex goes to #
    if(num_to==0)
      next
    for(j in 1:num_to){
      newedge <- c(i,curvec[j],curvec[j+num_to])
      output <- rbind(output,setNames(as.list(newedge), names(output)))
    }
    #convert numbers to letters
    for(m in 1:length(output$start)){
      for(n in 1:length(g)){
        if(output$start[m] == n)
          output$start[m] = replace(output$start[m],,names(g[n]))
        if(output$end[m] == n)
          output$end[m] = replace(output$end[m],,names(g[n]))
      } 
    }
  }
  return(output)
}

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
