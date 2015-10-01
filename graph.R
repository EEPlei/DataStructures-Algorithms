#edge function - helper#
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
    # makes a vector where you take the start node, end node, and the weight #
    # row bind the empty dataframe and set the names as the just made vectors #
    # and the names already assigned to the empty dataframe #
    #convert numbers to letters #
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
#is_valid function
is_valid <- function(g){
  if(typeof(g)!="list")
    return(FALSE)
  #check if the graph is a list
  if(length(g) == 0)
    return(FALSE)
  if(length(g) != length(unique(names(g))))
    return(FALSE)
  #check if the names for every element are all unique
  for(i in 1:length(g)){
    if(typeof(g[[i]]) != "list")
      return(FALSE)
    #check if every element is a list
    if(!("edges" %in% names(g[[i]]) & "weights" %in% names(g[[i]])))
      return(FALSE)
    if(length(g[[i]]$edges) | length(g[[i]]$weights) > 0)
      if(any(is.na(g[[i]]$edges)) | any(is.na(g[[i]]$weights)))
        return(FALSE)
    if(typeof(g[[i]]$edges) != "integer" & typeof(g[[i]]$edges) != "NULL")
      return(FALSE)
    if(class(g[[i]]$weights) != "numeric" & class(g[[i]]$weights) != "NULL")
      return(FALSE)
    #check if every element only contains edges and weights that are of the appropriate type
    if(any(g[[i]]$edges>length(g)) | any(g[[i]]$edges<1))
      return(FALSE)
    #check there are any edges to non-existent vertices
    if(any(g[[i]]$weights <= 0))
      return(FALSE)
    #check if all weights are not less than or equal to 0
    if(length(g[[i]]$edges) != length(g[[i]]$weights))
      return(FALSE)
    if(length(g[[i]]$edges) != length(unique(g[[i]]$edges)))
      return(FALSE)
    #check if every edge has a weight
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




#is_isomorphic

is_isomorphic <- function(g1, g2){
  
  if(!is_valid(g1)){
    stop("Please input a valid graph")
  } #tests is_valid#
  if(!is_valid(g2)){
    stop("Please input a valid graph")
  } #tests is_valid#
  df1 = edge(g1) # makes graph1 into dataframe #
  df2 = edge(g2) # makes graph2 into dataframe #
  if(nrow(df1) != nrow(df2)){
    return(FALSE)
  } # compares number of rows #
  if(nrow(df1) == 0){ # graph with empty list for vector #
    if(names(g1) != names(g2)){
      return(FALSE)
    } #compares name of vertex #
    return(TRUE) #if name of vertex matches, same #
  }
  if(nrow(df1)>0){
    for(i in 1:nrow(df1)){
      start <- df1$start[i] #start vertex in graph1 #
      end <- df1$end[i] # end vertex in graph1 #
      weight <- df1$weight[i] # weight of edge in graph1 #
      if(!(any(start %in% df2$start))){
        return(FALSE) 
      } # if name of start vertex isn't a starting vertex in graph2, stop #
      index.start = which(start == df2$start)
      # which rows in df2 starts with the same vertex as df1 #
      if(!(any(end == df2$end[index.start]))) {
        return(FALSE)
      } # if name of end vertex isn't a ending vertex of the rows index.start, stop #
      index.end = which(end == df2$end[index.start])
      # which row of index.start ends with the same vertex as df2 #
      # index.end shouldn't be a vector. it should just be a number #
      row.weight = index.start[index.end]
      if(weight != df2$weight[row.weight]){
        return(FALSE)
      }
    }
  } 
  return(TRUE)
}  





#is_connected function
is_connected <- function(g, v1, v2) {
  if(!is_valid(g)){ #if graph is not valid, return an error
    stop("Please input a valid graph")
  }
  
  if(length(v1)!=1 | length(v2)!=1){
    stop("Please input a non-vectorized input value in v1 or v2")
  }
  
  
  if(any(is.na(v1)) | any(is.na(v2))){ #if v1 or v2 are null, then return false 
    stop("Either v1 or v2 is empty please input a character value")
  }
  if(is.logical(v1) | is.logical(v2)){
    stop("Either v1 or v2 contains a logical vector please input a character value")
  }
  if(is.numeric(v1) | is.numeric(v2)){
    stop("Either v1 or v2 is a numeric value please input a character value")
  }
  
  if(!(any(v1 %in% names(g))) | (!(any(v2 %in% names(g))))){ 
    stop("Either v1 or v2  is not in graph")
  }
  
  
  edges <- edge(g)
  return(is_connected_helper(edges, v1, v2, c())) 
  # call up is_connected_helper, to use the "seen" array : this is to prevent infinite loops
}

is_connected_helper <- function(edges, v1, v2, seen) {
  #"seen" array denotes the vertices that I've already passed 
  if (length(seen) > 0) { #loop continues even when seen =0..weird
    if (v1 %in% seen){
      return(FALSE) # return FALSE if we've already passed the "seen" vector with our v1
    }
  }
  
  seen <- c(v1, seen) # insert v1 in the "seen" array
  if (nrow(edges)>0) #again, the array starts at 0 so need to specify to start at 1
    for (i in 1:nrow(edges)) {
      istart <- edges[i,1] # start column vector
      iend <- edges[i,2] #end column vector
      if (istart == v1 & (iend == v2 | is_connected_helper(edges, iend, v2, seen))) { 
        #1) if edge start is v1 and edge end is v2 
        #  or
        #2) if edge start is v1 and edge end is some other vertex other than v2 
        return(TRUE)
      }
    }
  return(FALSE)
}


shortest_path <- function(g,v1,v2){
  if(!is_valid(g)){
    stop("Please input a valid graph.")
  }
  
  
  if(!(is.character(v1) & is.character(v2))){
    stop("Bad Label")
  }
  
  
  if(!(v1 %in% names(g)) | !(v2 %in% names(g))){
    stop("The vertex/vertices you input is not valid!")
  }#test whether both vertices are valid inputs
  
  #the following code will apply Dijkstra's Algorithm
  edges <- edge(g)
  #dist means distance from source to source
  #prev means previous node in optimal path
  #initializaiton: vinfo is a table summarizing all vertices with dist and pre
  vinfo <- data.frame(v = character(),dist = numeric(),
                      prev = character(),stringsAsFactors = FALSE)
  #initially all nodes are unvisited
  unvisited <- names(g)
  #looping through the unvisited set to update all vertices so that their dist is infinity and previous node is undfined
  for(vert in unvisited){
    newvert <- data.frame(v=as.character(vert),dist=Inf,prev="undefined",stringsAsFactors=FALSE)
    vinfo <- rbind(vinfo, newvert)
  }
  
  #set the dist for v1 as zero first
  vinfo[vinfo$v == v1,]$dist <- 0
  
  #as long as there is still at least a not unvisited, loop through the nodes
  while(length(unvisited)!=0){
    #temp is an intermediate subtable of vinfo comprised of unvisited nodes
    temp <- vinfo[vinfo$v %in% unvisited,]
    u <- temp[temp$dist == min(temp$dist),][1,]$v   #vertex in unvisited with min dist[u]  
    if(temp[temp$v == u,]$dist == Inf){  #if u's dist is Inf, then it must be unconnectable because it already has the smallest dist among the unvisited set
      if(v2 %in% unvisited)
        #since a vertex with smallest dist has Inf dist, if now v2 is still unvisited, then it must be unreachable, return NULL
        #Actually here I could have use is_connected function but I forgot...
        return(c())
    }
    unvisited <- unvisited[unvisited != u]          #remove u from unvisited
    
    for(vert in edges[edges$start == u,]$end){      #for each neighbor u can go to
      d <- vinfo[vinfo$v == u,]$dist + edges[edges$start == u & edges$end == vert,]$weight
      # let d be the sum of dist between u to vert and u's dist
      if(d < vinfo[vinfo$v == vert,]$dist){           # If d is less than vert's dist, then a shorter path to vert has been found
        vinfo[vinfo$v == vert,]$dist <- d  #update vert's dist as d
        vinfo[vinfo$v == vert,]$prev <- u  #update vert's prev as u
      }
    }
  }
  
  if(v1 != v2){  
    path <- v2  # let path be a vector to store the output in reverse order, and initially put v2 in it
    vert <- v2  # let vert be the last node on the optimal path which has been put into path
    while(vinfo[vinfo$v == vert,]$prev != "undefined"){ #if vert's pre is undefined, then stop because it has reached v1
      path <- c(path,vinfo[vinfo$v == vert,]$prev)   #put pre into path recursively
      vert <- vinfo[vinfo$v == vert,]$prev           #and update vert recursively
    }
    
    return(rev(path))  # finally return the reverse of path
  }else{
    #this part is a bit painful because at first I didn't think v1 can be equal to v2
    #therefore in my previous code design I use undefined prev for v1
    #when I found this error, I was approaching the deadline so I chose not to touch previous code and add the 
    #additional consideration for the case v1 = v2 here.
    #so to answer Prof. Rundel's question, is the painful code below really necessary?
    #should have been no, but yes when we approached deadline. sorry about that, if you're patient, please finish reading it smile emoticon
    toV1 <- edges[edges$end == v1,]
    #let toV1 store the edges whose end is v1 (then is v2 too)
    minlength <- Inf
    #minlengh is the shortest path's distance 
    penultimate <- NULL
    #penultimate is the second last vertex in the shortest path, i.e. the vertex pointing back to v1
    for(element in toV1[toV1$start != v1,]$start){  #loop through the vertices who points to v1 excluding v1 itself, call it element
      if(toV1[toV1$start == element,]$weight + vinfo[vinfo$v == element,]$dist < minlength){
        #if the dist between element and v1 plus the dist of element is less than minlength, then update it as the new minlength
        minlength <- toV1[toV1$start == element,]$weight + vinfo[vinfo$v == element,]$dist
        penultimate <- element
        # update the penultimate vertex
      }
    }
    if(v1 %in% toV1$start){
      #after update minlength and penultimate, if v1 can also walk to itself directly
      if(toV1[toV1$start == v1,]$weight <= minlength){
        #then if it's distance to itself is less than minlenght
        return(c(v1,v1))
        #output v1 to v1 directly
      }else{
        #otherwise, if penultimate is null, then v1 cannot go back to itself
        if(is.null(penultimate)){
          return(c())
        }else{
          #otherwise, penultimate exsits, update path and vert in the same way as was done when v1 !=v1
          path <- penultimate
          vert <- penultimate
          while(vinfo[vinfo$v == vert,]$prev != "undefined"){
            path <- c(path,vinfo[vinfo$v == vert,]$prev)
            vert <- vinfo[vinfo$v == vert,]$prev
          }
          
          return(c(rev(path),v1))
        }
      }
    }else{
      #otherwise v1 cannot walk to itself directly
      if(is.null(penultimate)){
        #if penultimate is also null at the same time, then return null
        return(c())
      }else{ # otherwise, same output return as before
        
        path <- penultimate
        vert <- penultimate
        while(vinfo[vinfo$v == vert,]$prev != "undefined"){
          path <- c(path,vinfo[vinfo$v == vert,]$prev)
          vert <- vinfo[vinfo$v == vert,]$prev
        }
        
        return(c(rev(path),v1))
      }
    }
  }
}



