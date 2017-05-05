library(igraph)

simulate_caveman <- function(n = 25, clique_size = 5){
  require(igraph)
  # Groups are all the same size, so I check whether N is divisible by the size of groups
  if ( ((n%/%clique_size) * clique_size) != n){
    stop("n is not evenly divisible by clique_size")
  }
  
  groups = n/clique_size # this determines the number of groups
  
  el <- data.frame(PersonA = 1:n, Group = NA) # I create a dataframe which has people and the groups they are in
  # I treat it like a person to group edgelist
  
  group_vector = c()
  for (i in 1:groups){
    group_vector <- c(group_vector, rep(i, clique_size))
  }  
  
  el$Group <- group_vector
  
  inc <- table(el) # I use the table function to turn the person to group edgelist into an incidence matrix
  adj <- inc %*% t(inc) # And I use matrix multiplication with the transpose to turn the person to group incidence matrix
  # into a person to person adjacency matrix
  
  diag(adj) <- 0 
  
  g <- graph.adjacency(adj, mode = "undirected") # I graph this matrix
  
  group_connect <- seq(from = 1, to = n, by = clique_size) # I determine the points of connection using a sequence funciton
  
  for( i in 1:(length(group_connect)-1)){
    p1 <- group_connect[i] + 1
    p2 <- group_connect[i+1]
    g <- add.edges(g, c(p1,p2)) # And I connect the points of connection using add.edges
  }
  g <- add.edges(g, c(group_connect[1],(group_connect[groups]+1))) # finally I connect the ends of the structure so that it forms a circle
  
  return(g)    
}


caveman_net <- simulate_caveman(n = 100, clique_size = 5) 
par(mar = c(2,2,2,2))

kk_layout =  layout.kamada.kawai(caveman_net)

plot(caveman_net, layout = kk_layout, vertex.size = 2, vertex.label = NA, vertex.color = "grey80")

geographic_dist = as.matrix(dist(kk_layout))
social_dist = as.matrix(distances(caveman_net, v = V(caveman_net), to = V(caveman_net)))

sim_geo = function(graph, dist_mat = geographic_dist, geographic = T, iter = 1000){
  
  for (i in 1:iter){
    graph_adj = get.adjacency(graph)
    graph_adj = as.matrix(graph_adj)
    
    sampled_node = sample(1:vcount(graph), 1)
    
    not_present = graph_adj[sampled_node,]
    
    ## select node
    
    
    
    ## delete edge
    
    
    
    ## reform edge
    
    probs = probs[sampled_node, ]/sum(probs[sampled_node, ])
    
    
  }
  
  
  
  
  
  
  
  
  
  
}




