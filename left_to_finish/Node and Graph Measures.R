# ****************************************************************
# Node and Network Measures Tutorial: 10/27/2016
# ****************************************************************

# In network analysis we can fluidly move between different levels of analysis, characterizing individuals by their positions in networks and networks by the patterning of their social relations. In this tutorial, we look at individual-levels of position and their graph-level analogues. We begin by learing about four different centrality measures: degree centrality, betweenness centrality, closeness centrality, and eigenvector centrality.

# Centrality measures all try to identify important actors in a network. There are different measures because they capture different aspects or definitions of what makes an actor important. A million different measures have been proposed but the four we learn here are the most commonly famous.

# We then learn about the graph-level analogues of centrality measures: centralization and the degree distribution. Centralization measures the extent to which the ties of a given network are concentrated on a single actor or group of actors, while the degree distribution, a simple histogram of degree, can tell you whether network degree is highly unequally distributed or not.

# Finally, we look at other characteristics of graphs, such as clustering (a graph-level analogue of the triad closure), diameter and average path length.

# ****************************************************************
# Loading the example network
# ****************************************************************

# As always, we need to load igraph.
library(igraph)

# The dataset we are loading is called padgett_marriage.csv 

# It is a subset of a famous dataset collected by John Padgett on the relationships 
# of prominent Florentine families in 15th century Italy. The historical puzzle surrounding
# the data is that the Medici, an upstart family, seizes political power in 1434.
# Padgett's goal was to explain how the Medici rose to power. 

# He looks at many relations, but we are looking only at marriage. 
# Marriage was a tool in diplomacy, and was central to political alliances.
# A tie is drawn between families if the daughter of one family was sent to marry the 
# son of another. Directionality follows the flow of daughters

# As always, we first load and prepare the dataset. 
attributes <- read.csv("~/Documents/Padget116attributes.csv", stringsAsFactors = FALSE)

# PARTNER
padgett_marriage <- read.graph("~/Documents/marriage116.paj", format = c("pajek"))
V(padgett_marriage)$name <- 1:length(V(padgett_marriage))
V(padgett_marriage)$name <- attributes$Family[match(attributes$Order, V(padgett_marriage)$name)]
padgett_marriage <- get.edgelist(padgett_marriage)
marriageNet <- graph_from_data_frame(padgett_marriage, directed = TRUE, vertices = attributes[,-1])

# Let's see how it looks.
plot(marriageNet, vertex.size = 5, vertex.label.cex = .8, vertex.label.color = "black", edge.arrow.size = .4)

# Based on this plot - which family do you expect is most central?

# ****************************************************************
# Degree Centrality
# ****************************************************************

# The simplest measure of centrality is degree centrality. 
# It counts how many edges each node has - the most degree central actor
# is the one with the most ties. 

# (In a directed network, you will need to specify if in or out ties should be counted
# These will be referred to as in or out degree respectively. If both are counted, then
# it is called degree centrality)

# Degree centrality is calculated using the degree function in R.

degree(marriageNet) # It returns how many edges each node has. 

# Who is the most degree central? 

# We can assign the output to a variable in the network and size the nodes according
# to degree.

V(marriageNet)$degree <- degree(marriageNet) # assignment

plot(marriageNet, vertex.size = V(marriageNet)$degree, vertex.label.cex = .8, vertex.label.color = "black", edge.arrow.size = .4)

# The problem is that the degree values are too large for some families.
# We can use a scalar to decrease the value of the degree but maintain the ratio.

plot(marriageNet, vertex.size = V(marriageNet)$degree * .3, vertex.label.cex = .8, vertex.label.color = "black", edge.arrow.size = .4)

# ****************************************************************
# Betweenness Centrality
# ****************************************************************

# Betweenness centrality captures which nodes are important in the flow of the network.
# It makes use of the shortest paths in the network.
# A path is a series of adjacent nodes. 

# For any two nodes we can find the shortest path between them, that is, the path
# with the least amount of total steps (or edges)

# If a node C is on a shortest path between A and B, then it means C is important 
# to the efficient flow of goods between A and B.  Without C, things would have to
# take a longer route to get from A to B. 

# Thus, betweenness effectively counts how many shortest paths each node is on. 
# The higher a nodes betweenness, the more important they are for the flow of goods
# in a network.

# betweenness() computes betweenness in the network

betweenness(marriageNet, directed = TRUE)

# ACCIAIUOL   ALBIZZI BARBADORI  BISCHERI CASTELLAN    GINORI  GUADAGNI LAMBERTES 
# 0.000000 19.333333  8.500000  9.500000  5.000000  0.000000 23.166667  0.000000 

# MEDICI     PAZZI   PERUZZI     PUCCI   RIDOLFI  SALVIATI   STROZZI TORNABUON 
# 47.500000  0.000000  2.000000  0.000000 10.333333 13.000000  9.333333  8.333333

# We can again assign the output of betweenness() to a variable in the network 
# and size the nodes according to it.

V(marriageNet)$betweenness <- betweenness(marriageNet, directed = TRUE) # assignment

plot(marriageNet, vertex.size = V(marriageNet)$betweenness * .3, vertex.label.cex = .8, vertex.label.color = "black", edge.arrow.size = .4)

# Betweenness centrality can be very large. It is often helpful to normalize it by dividing
# by the maximum and then multiplying by some scalar when plotting. 

V(marriageNet)$betweenness = V(marriageNet)$betweenness/max(V(marriageNet)$betweenness)

plot(marriageNet, vertex.size = V(marriageNet)$betweenness * 10, vertex.label.cex = .8, vertex.label.color = "black", edge.arrow.size = .4)

# ****************************************************************
# Closeness Centrality
# ****************************************************************

# With closeness centrality we again make use of the shortest paths between nodes.
# We measure the distance between two nodes as the length of the shortest path between them.
# Farness, for a given node, is the average distance from that node to all other nodes.
# Closeness is then the reciprocal of farness (1/farness).

closeness(marriageNet)

# ACCIAIUOL     ALBIZZI   BARBADORI    BISCHERI   CASTELLAN      GINORI    GUADAGNI 
# 0.018518519 0.022222222 0.020833333 0.019607843 0.019230769 0.017241379 0.021739130 
# LAMBERTES      MEDICI       PAZZI     PERUZZI       PUCCI     RIDOLFI    SALVIATI 
# 0.016949153 0.024390244 0.015384615 0.018518519 0.004166667 0.022727273 0.019230769 
# STROZZI   TORNABUON 
# 0.020833333 0.022222222 

# We assign it to a node variable and plot the network, adjusting node size by closeness
V(marriageNet)$closeness <- closeness(marriageNet, mode = "out")

V(marriageNet)$closeness = V(marriageNet)$closeness/max(V(marriageNet)$closeness)

plot(marriageNet, vertex.size = V(marriageNet)$closeness * 10, vertex.label.cex = .8, vertex.label.color = "black", edge.arrow.size = .4)

# ****************************************************************
# Eigenvector Centrality
# ****************************************************************

# Degree centrality only takes into account the number of edges for each node.
# but it leaves out information about ego's alters.

# However, we might think that power comes from being tied to powerful people. 
# If A and B have the same degree centrality, but A is tied to all high degree people
# and B is tied to all low degree people, then intuitively we want to see A with a higher
# score than B. 

# Eigenvector centrality takes into account alters' power. 
# It is calculated a little bit differently in igraph.
# It produces a list object and we need to extract only the vector of values

evcent(marriageNet)$vector

#  3.071155e-01 5.669336e-01 4.919853e-01 6.572037e-01 6.019551e-01 1.741141e-01
#  6.718805e-01 2.063449e-01 1.000000e+00 1.041427e-01 6.407743e-01 9.567041e-18
#  7.937398e-01 3.390994e-01 8.272688e-01 7.572302e-01

# Then we can assign it to our network and plot

V(marriageNet)$eigenvector <- evcent(marriageNet)$vector
V(marriageNet)$eigenvector = V(marriageNet)$eigenvector/max(V(marriageNet)$eigenvector)

plot(marriageNet, vertex.size = V(marriageNet)$eigenvector * 10, vertex.label.cex = .8, vertex.label.color = "black", edge.arrow.size = .4)

# We can use the function bonpow() for the same purpose.  However, Bonacich cleverly
# notes that being connected to powerful people can be a bad thing in certain contexts.
# Can you think of context where this might be the case?

# He proposes a measure that can be manipulated to accomodate cases where having
# powerful friends is either helpful or hurtful. If the exponent is negative, 
# alters' having higher centrality score will hurt ego's score; and vise versa.

bonpow(marriageNet, exponent = .5)
bonpow(marriageNet, exponent = -.5)

# ****************************************************************
# Measure Correlations
# ****************************************************************

# Most of these measures are highly correlated, meaning they don't necessarily capture
# unique aspects of pwoer. 

# However, the amount of correlation depends on the network structure. 

# Let's see how the correlations between centrality measures looks in the 
# Florentine Family network.

# cor.test(x,y) does a simple correlation test for two vectors

cor.test(V(marriageNet)$degree, V(marriageNet)$betweenness) # 0.93
cor.test(V(marriageNet)$degree, V(marriageNet)$eigenvector) # 0.92
cor.test(V(marriageNet)$degree, V(marriageNet)$closeness) # 0.37
cor.test(V(marriageNet)$betweenness, V(marriageNet)$eigenvector) # .79
cor.test(V(marriageNet)$betweenness, V(marriageNet)$closeness) # .24

# What do we learn?

# ****************************************************************
# Centralization and Degree Distributions
# ****************************************************************

# To understand the measures further, we can look at their distributions
# This will tell us roughly how many nodes have centralities of a given value

hist(V(marriageNet)$betweenness)

# We can see that most nodes in the marriage network have low betweenness
# centrality, and only one node has more than 40 betweenness. 

# Degree distributions tend to be right-skewed; that is, only a few nodes in most
# networks have most of the ties. 

# Evenly distributed degree is rarer, more surprising and perhaps more interesting.

# Finally centralization measures the extent to which a network is centered around
# a single node. The closer a network gets to looking like a star, the more centralization will be. 

centralization.degree(marriageNet)$centralization
centralization.betweenness(marriageNet)$centralization
centralization.evcent(marriageNet)$centralization
centralization.closeness(marriageNet)$centralization

# ************************************************************************
# Small-world measures
# ************************************************************************

# Small-worlds have high clustering and low average path length.  To test whether your network is a small world, you can look at the clustering and average path length in your network and see how high the values are.

transitivity(marriageNet) # transitivity() measures clustering coefficient, which essentially says, how clustered is the network overall
# the marriage network has low transitivity

average.path.length(marriageNet)# Path length is low - basically it takes 3.3 steps, on average, to reach one node from a random other node

# Finally we can plot the diameter of the network to see how far apart the farthest two actors are from each other. The diameter is the longest shortest path. 
nodes.diameter <- get.diameter(marriageNet) # This gets the nodes in the diameter
V(marriageNet)$color<-"white"
V(marriageNet)[nodes.diameter]$color<-"darkgreen" # This assigns those nodes the color darkgreen.
plot(marriageNet, vertex.size = log(betweenness(marriageNet)/2), vertex.label = NA) # Now you can clearly see what a caveman structure is.

