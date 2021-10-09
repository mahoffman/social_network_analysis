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

sim_geo = function(graph, dist_mat = geographic_dist, iter = 1000){
  
  densities = graph.density(graph)
  transitivities = transitivity(graph) 
  path_length = average.path.length(graph)
  
  for (i in 1:iter){
    ## select node
    sampled_node = sample(1:vcount(graph), 1)
    
    ## delete edge
    edge_to_delete = sample(incident(graph, sampled_node),1)
    if(length(edge_to_delete) > 0){
      graph = delete.edges(graph, edge_to_delete)
    }
    
    ## reform edge
    graph_adj = get.adjacency(graph)
    graph_adj = as.matrix(graph_adj)
    
    not_present = graph_adj[sampled_node,]
    not_present = ifelse(not_present == 0, 1, 0)
    
    probs = (1/dist_mat[sampled_node,])
    probs = probs * not_present
    
    probs = ifelse(is.infinite(probs), 0, probs)
    probs = probs/sum(probs)
    
    to_add = sample(names(probs), 1, prob = c(probs))
    
    graph = add_edges(graph, c(sampled_node, to_add))
    
    densities = c(densities, graph.density(graph))
    transitivities = c(transitivities, transitivity(graph))
    path_length = c(path_length, average.path.length(graph))
  }
  
  return(list(graph = graph, stats = list(Iter = 0:iter, Density = densities, Transitivity = transitivities, PathLengths = path_length)))
}


caveman_net <- simulate_caveman(n = 300, clique_size = 30) 
par(mar = c(2,2,2,2))

kk_layout =  layout.kamada.kawai(caveman_net)

plot(caveman_net, layout = kk_layout, vertex.size = 2, vertex.label = NA, vertex.color = "grey80")


## distances weighted by geographic proximity
geographic_dist = as.matrix(dist(kk_layout))

trial = sim_geo(caveman_net, dist_mat = geographic_dist, iter = 500)

plot(trial$stats$Iter, trial$stats$PathLengths/max(trial$stats$PathLengths), type = "l", col = "red", lwd = 4, ylim = c(0,1))
lines(trial$stats$Iter, trial$stats$Transitivity/max(trial$stats$Transitivity), type = "l", col = "blue", lwd = 4)

polygon(c(trial$stats$Iter,rev(trial$stats$Iter)),c(trial$stats$Transitivity/max(trial$stats$Transitivity),rev(trial$stats$PathLengths/max(trial$stats$PathLengths))),col="grey80", border = "grey80")


## distances weighted by geographic proximity squared
geographic_dist_squared = geographic_dist^3

trial = sim_geo(caveman_net, dist_mat = geographic_dist_squared, iter = 500)

plot(trial$stats$Iter, trial$stats$PathLengths/max(trial$stats$PathLengths), type = "l", col = "red", lwd = 4, ylim = c(0,1))
lines(trial$stats$Iter, trial$stats$Transitivity/max(trial$stats$Transitivity), type = "l", col = "blue", lwd = 4)

polygon(c(trial$stats$Iter,rev(trial$stats$Iter)),c(trial$stats$Transitivity/max(trial$stats$Transitivity),rev(trial$stats$PathLengths/max(trial$stats$PathLengths))),col="grey80", border = "grey80")


## distances weighted the same
all_ones = geographic_dist
all_ones[] = 1

trial = sim_geo(caveman_net, dist_mat = geographic_dist, iter = 500)

plot(trial$stats$Iter, trial$stats$PathLengths/max(trial$stats$PathLengths), type = "l", col = "red", lwd = 4, ylim = c(0,1))
lines(trial$stats$Iter, trial$stats$Transitivity/max(trial$stats$Transitivity), type = "l", col = "blue", lwd = 4)

polygon(c(trial$stats$Iter,rev(trial$stats$Iter)),c(trial$stats$Transitivity/max(trial$stats$Transitivity),rev(trial$stats$PathLengths/max(trial$stats$PathLengths))),col="grey80", border = "grey80")


par(mfrow = c(3, 3))

for(i in seq(from = 1, to = 5, by = .5)){
  geographic_dist_squared = geographic_dist^i
  
  trial = sim_geo(caveman_net, dist_mat = geographic_dist_squared, iter = 500)
  
  plot(trial$stats$Iter, trial$stats$PathLengths/max(trial$stats$PathLengths), type = "l", col = "red", lwd = 4, ylim = c(0,1), main = paste("Exponent:", i))
  lines(trial$stats$Iter, trial$stats$Transitivity/max(trial$stats$Transitivity), type = "l", col = "blue", lwd = 4)

}

