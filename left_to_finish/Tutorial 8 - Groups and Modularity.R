# ******************************************************************************************************************
# Finding Groups in Networks Tutorial
# ******************************************************************************************************************

# Groups are one of the many tools we have for analyzing network structure. Group detection
# focuses on the presence of ties - in attempt to identify densly connected groups of actors. 
# There are many different tools for doing this - we will cover four: component analysis, k-cliques, 
# modularity, and cohesive blocking.  Each method has its own uses and theoretical underpinnings, so
# I put citations of famous papers that use each method at the bottom of the script. 

# ******************************************************************************************************************
# Component analysis
# ******************************************************************************************************************

# The most basic form of group is a component.  In a connected component, every node is reachable via 
# some path by every other node. Most network datasets have only a single large connected component 
# with a few isolates - however, some unique datasets might have three or four large, distinct components.

# In a directed graph, components can be weakly or strongly connected. If node i can reach j via a 
# directed path and j can reach i via a directed path, for all i and j nodes in the component, then we
# say the component is strongly connected. If all nodes are only reachable from a single direction, 
# (i.e. i can reach j via a directed path, but j can't reach i), then we say the component is weakly
# connected.

# The ?decompose.graph function in igraph will take a network and decompose it into its connected components
# We can then analyze each component separately. 
library(igraph)

g1 <- barabasi.game(100, 1, 5)
V(g1)$name <- as.character(1:100)

g2 <- erdos.renyi.game(100, graph.density(g1), directed = TRUE)
V(g2)$name <- as.character(101:200)

g3 <- graph.union(g1, g2, byname = TRUE) # The ?graph.union function combines two networks. 

plot(g3, vertex.label = NA, vertex.size=2, edge.arrow.size = .1) # Two distinct networks in a single plot

# Now let's use decompose to isolate each component.  There will be three since there are two networks and
# a single isolate node.

component_list <- decompose.graph(g3, mode = "weak")
component_list # It returns a list with three graphs in it - one for each component

plot(component_list[[1]], main = "The Scale-Free Graph")
plot(component_list[[2]], main = "The Random Graph")
plot(component_list[[3]], main = "The Lonely Isolate")

# ******************************************************************************************************************
# Cliques
# ******************************************************************************************************************

# Cliques are fully connected subgraphs within a network structure; they are like the caves in the
# caveman structure we learned about a few weeks ago. 

# We often want to find all of the cliques in a network of various sizes.  We could have a theory
# for example that people will dress or behave similarly or affect those in their cliques. 
# That is, we might imagine cliques to be meaningful for the outcomes we are interested in study. 

# We can do this with the clique function in igraph. 

clique_out <- cliques(g1)
length(clique_out) # Wow... imagine the number of cliques in a large network

# If we want to look for cliques of a certain size we can use the min and max arguments.
clique_four <- cliques(g1, min = 4, max = 4)
length(clique_out) # Cliques of size four make about 25% of total cliques in the network.

# ******************************************************************************************************************
# Cohesive Blocking
# ******************************************************************************************************************

# Cohesive blocking builds on the idea of cliques.  It starts at the level of the component and identifies
# large substructures nested within the component. It then moves to those large substructures and identifies
# smaller and smaller nested substructures, until it reaches cliques. It is therefore a useful
# way to operationalize network embeddedness. 

# We will run it on a small world network size 100. 
g2 <- as.undirected(g2)
g4 <- watts.strogatz.game(1, 100 , 5, .1)
V(g4)$name <- as.character(1:100)

g5 <- graph.union(g2, g4)

# The function for cohesive blocking is ?cohesive.blocks. It is very inefficient so it will take awhile
# to run. Run time depends on the number of edges in the network and the degree of nestedness. 
# Don't even bother running it on very large networks.

blocks <- cohesive.blocks(g5)  # The basic function for finding cohesive blocks

blocks(blocks) # This tells us what the blocks are and which nodes are in them

cohesion(blocks) # This gives a cohesion score for each block. Cohesion is the minimum number 
                 # of vertices you must remove in order to make the block not strongly connected.

plotHierarchy(blocks) # Finally, plotHierarchy shows the nestedness structure of the blocks.

# ******************************************************************************************************************
# Group Detection
# ******************************************************************************************************************

# Often times, we just want to find distinct groups of people. We might not require that everyone
# in a group is connected - it is too stringent. Instead, we could define groups as sets of nodes 
# who have a higher proportion of edges going inwards than outwards - that is, solidarity is strongest
# within the group.

# There are a plethora of group detection algorithms in igraph. I will cover some of them.
# The full list is: edge.betweenness.community, fastgreedy.community, label.propagation.community, 
# leading.eigenvector.community, multilevel.community, optimal.community, spinglass.community, and walktrap.community.

# This webpage has a summary of their pros and cons for an older version of igraph: 
# http://bommaritollc.com/2012/06/summary-community-detection-algorithms-igraph-0-6/

# Essentially you should tailor your algorithm choice to your network. Certain algorithms are 
# designed for directed or undirected graphs, and work better with small or large graphs. 

# Each algorithm as its own igraph function. These functions produce lists with information about the algorithm results.
# Element 1 holds a vector that details which group each node is in, which I will refer to as the membership vector.
# Element 6 holds the modularity of the network given the group detection algorithm . 

# For undirected graphs, you can use the optimal or multilevel algorithms.
communityMulti <- multilevel.community(g4) # this performs best for undirected

# For directed graphs, edge betweenness is generally your best bet, though the walktrap algorithm performs well too.
communityWalk <- walktrap.community(g2)
communityEB <- edge.betweenness.community(g2)
communityInfo <- infomap.community(g2)

# We might wish to visualize the group structure of networks to see how the algorithms divide the network.
# To do so, we have to first extract the membership vector and put it in a format the plot function understands.
communityMultiGroup <- by(seq_along(communityMulti$membership), communityMulti$membership, invisible)
communityMultiGroup
# We can then use the mark.groups argument in plot to see the groups. 
plot(g4, vertex.size = 3, vertex.label = NA, mark.groups = communityMultiGroup)

# Alternatively, we can just color the nodes by group membership.  In this case, we can just use the membership vector.
V(g4)$color <- communityMulti$membership
plot(g4, vertex.size = 3, vertex.label = NA)

# This becomes less feasible as the number of groups increases!!

# ******************************************************************************************************************
# Modularity
# ******************************************************************************************************************


# Modularity takes a given group structure and calculates how separated the different groups are from each other.
# It therefore operationalizes our notion of groups above by calculating the proportion of ties that
# are within groups as opposed to between them. Networks with high modularity have dense connections between the nodes within modules
# but sparse connections between nodes in different modules.


# We can think of modularity as both a measure of how effective a given grouping algorithm is - 
# i.e. higher modularity means the alogrithm is identifying distinct, sociall separate groups.

# But it can also be thought of as a measure of the saliency of groups to the network in general. 
# The higher modularity the more that groups structure the network. 

# Modularity measures the extent to which a network can be divided into distinct modules or groups.

# Getting a network's modularity in igraph is easy! 

communityMulti <- multilevel.community(g4)
communityMulti$modularity # We can either access the modularity score directly 


# OR 

modularity(communityWalk) # We can use the modularity() function on a group detection output.