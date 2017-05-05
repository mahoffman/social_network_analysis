# A basic function which prevents the formation of specified triads in a random graph simulation
banning_triads_game = function(n = 100, porm = .05, banned = c(2), sim_max = 1000000, probrecip = .5){
  
  require(igraph) # Ensures igraph is loaded
  
  if(any(c(1) %in% banned)){
    stop("Can't ban 003s") # Stops the simulation if the user tried to bad 003 or 012 triads
  }
  
  num_edges = round(n*(n-1)*porm, 2) # calculates the desired number of edges according to the N and Porm parameters
  
  net = make_empty_graph(n = n, directed = TRUE) # initializes an empty network

  edge_count = 0
  sim_count = 0
  
  while(edge_count < num_edges){ # Begins a loop, which ends once the requisite number of edges is reached. 
    
    # This part samples two nodes, checks whether the two sampled nodes are the same node, and whether an edge is already present in the network between these nodes
    
    uniq = TRUE
    edge_present = TRUE
    while(uniq == TRUE | edge_present == TRUE){
      edge_id = sample(1:n, 2, replace = T)
      uniq = edge_id[1] == edge_id[2]
      reciprocated = sample(c(FALSE, TRUE), 1, prob = c(1-probrecip, probrecip))
      edge_present_1 = are.connected(net, edge_id[1], edge_id[2])
      if (reciprocated){
        edge_present_2 = are.connected(net, edge_id[2], edge_id[1])
        edge_present = edge_present_1|edge_present_2
      } else {
        edge_present = edge_present_1
      }
    }
    
    # Calculates the traid census for the network before adding an edge
    before = triad.census(net)
    net_new = net + edge(edge_id) # Adds in the edge
    if(reciprocated){
      edge_id_rev = edge_id[2:1]
      net_new = net_new + edge(edge_id_rev) # Adds in the edge
    }
    after = triad.census(net_new) # Calculates the triad census again
    triad_diff = after - before # Checks to see how much the triad census changed
    
    if(all(triad_diff[banned] == 0)){
      net = net_new # If the banned triads still aren't observed, then the new network is accepted.
    }
    
    edge_count = ecount(net) # number of edges updated
    sim_count = sim_count + 1 # Simulation count updated
    if(sim_count > sim_max){
      print("Warning: Failed to converge, banned triads may be incompatible") # exits simulation if simulation max count is exceeded
      return(net)
    }
  }
  return(net) # Returns the simulated network
}







n = 100, porm = .05, banned = c(2), sim_max = 1000000, probrecip = .5


result = banning_triads_game(banned = c(4,7,9,11), probrecip = .1)
plot(result, vertex.size = 2, vertex.label = NA, edge.arrow.size = .1)
