library(igraph)
money_edgelist = as.matrix(money_edgelist)
moneyNetwork = graph.edgelist(money_edgelist, directed = TRUE)

plot(moneyNetwork)

plot(moneyNetwork, vertex.size = 7, vertex.color = "tomato", vertex.label.color = "black", edge.width = .3, edge.arrow.size = .3, edge.color = "grey10", vertex.label.cex = .85)


money_edgelist = read.csv("money_edgelist.csv", stringsAsFactors = F)
attributes = read.csv("attributes2.csv", stringsAsFactors = FALSE)

moneyNetwork = graph.data.frame(money_edgelist, directed = T, vertices = attributes)