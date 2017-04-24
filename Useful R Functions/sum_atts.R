#####  ##############################################################
# 1 #   sum_atts
#####  ##############################################################
# Function that gets each ego's list of alters and either finds the 
# average attribute of alters or counts how many alters of attr == value
# each ego has. 

# g requires an igraph object. attr should be a vector of attributes for
# nodes in a network; therefore, it must be the same length as vcount(g)

# if average is FALSE, then the function sums the number of 
# alters == value; otherwise it finds the average value of attr 
# for egos' alters

# value should equal one of the possible values in attr. it counts
# how many alters are equal to value. This only matters when
# average == FALSE

# mode determines whether alters are undirected vs. directed alters.
# if it is all, any node tied to ego is treated as an alter. 

# prop determines whether the results should be divided by the degree
# of ego. This only matters when average == FALSE.
sum_atts <- function(g, attr = V(g)$attr, average = FALSE, value = 1, mode = "all",  prop = TRUE){
  
  require(igraph)
  
  if( is.igraph(g) == FALSE) stop("g must be an igraph object")
  
  if (attrs == ""){
    stop("Missing attribute argument")
  }
  
  
  V(g)$temp <- attr
  
  if (average == TRUE){
    result <- unlist(lapply(get.adjlist(g, mode=mode), function (neis) { sum(V(g)[neis]$temp, na.rm=T)})) / degree(g)
  } else {
    V(g)$weight <- 1
    if (prop == TRUE){
      result <- unlist(lapply(get.adjlist(g, mode=mode), function (neis) { sum(V(g)[neis]$weight[V(g)[neis]$temp == value], na.rm=T)})) / degree(g)
    } else {
      result <- unlist(lapply(get.adjlist(g, mode=mode), function (neis) { sum(V(g)[neis]$weight[V(g)[neis]$temp == value], na.rm=T)}))
    }
  }
  return(result)
}