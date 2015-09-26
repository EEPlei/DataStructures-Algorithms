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
      if(edges[edges$start == iend & edges$end == istart,]$weight != iweight)
        return(FALSE)
    } 
  return(TRUE)
}




#is_isomorphic




#is_connected function
is_connected <- function(g, v1, v2) {
  if(!is_valid(g)){ #if graph is not valid, return an error
    stop("Please input a valid graph")
  }
  if(is.na(v1) | is.na(v2)){ #if v1 or v2 are null, then return false 
    stop("Either v1 or v2 is empty please input a character value")
  }
  if(is.logical(v1) | is.logical(v2)){
    stop("Either v1 or v2 contains a logical vector please input a character value")
  }
  if(is.numeric(v1) | is.numeric(v2)){
    stop("Either v1 or v2 is a numeric value please input a character value")
  }
  
  if(!(v1 %in% names(g))){
    stop("v1 is not in graph")
  }
  
  if(!(v2 %in% names(g))){
    stop("v2 is not in graph")
  }
  
  edges <- edge(g)
  return(is_connected_helper(edges, v1, v2, c())) 
  # call up is_connected_helper, to use the "seen" array : this is to prevent infinite loops
}

is_connected_helper <- function(edges, v1, v2, seen) {
  #"seen" array denotes the vertices that I've already passed 
  if (length(seen) > 0) { #loop continues even when seen =0..weird
    for (i in 1:length(seen)) {
      if (v1 == seen[i]) {
        return(FALSE) # return FALSE if we've already passed the "seen" vector with our v1
      }
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
