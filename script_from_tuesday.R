library(igraph)
money_edgelist = as.matrix(money_edgelist)
moneyNetwork = graph.edgelist(money_edgelist, directed = TRUE)

plot(moneyNetwork)

plot(moneyNetwork, vertex.size = 7, vertex.color = "tomato", vertex.label.color = "black", edge.width = .3, edge.arrow.size = .3, edge.color = "grey10", vertex.label.cex = .85)

library(igraph)

money_edgelist = read.csv("money_edgelist.csv", stringsAsFactors = F)
attributes = read.csv("attribute_df.csv", stringsAsFactors = FALSE)

moneyNetwork = graph.data.frame(money_edgelist, directed = T, vertices = attributes)







plot(moneyNetwork)

moneyNetwork

# The V function allows you to operate on the vertices in the network.
V(moneyNetwork)$Gender = c("Male", "Male", "Male", "Male", "Male", "Male")
V(moneyNetwork)$Degree = degree(moneyNetwork)
V(moneyNetwork)$Indegree = degree(moneyNetwork, mode = "in")
V(moneyNetwork)$Outdegree = degree(moneyNetwork, mode = "out")

attributes$Degree = degree(moneyNetwork)


V(moneyNetwork)$color = c("tomato", "tomato", "blue", "blue", "blue", "blue")

V(moneyNetwork)$color = ifelse(V(moneyNetwork)$Role %in% c("Father", "Mother"), "tomato", "blue")

V(moneyNetwork)$color = ifelse(V(moneyNetwork)$Role == "Father" | V(moneyNetwork)$Role == "Mother", "tomato", "blue")


V(moneyNetwork)$color = "blue"
V(moneyNetwork)$color = ifelse(V(moneyNetwork)$Role == "Father", "tomato", V(moneyNetwork)$color)
V(moneyNetwork)$color = ifelse(V(moneyNetwork)$Role == "Mother", "green", V(moneyNetwork)$color)
V(moneyNetwork)$color = ifelse(V(moneyNetwork)$Role == "Son", "pink", V(moneyNetwork)$color)


V(moneyNetwork)$color = as.numeric(as.factor(V(moneyNetwork)$Role))

table(V(moneyNetwork)$color, V(moneyNetwork)$Role)

V(moneyNetwork)$size = degree(moneyNetwork, mode = "in")*5
V(moneyNetwork)$size = V(moneyNetwork)$Age

plot(moneyNetwork)

colfunc <- colorRampPalette(c("white", "red"))
gradient <- colfunc(5)

V(moneyNetwork)$color = gradient[(degree(moneyNetwork, mode = "in")+1)]


V(moneyNetwork)$frame.color = ifelse(V(moneyNetwork)$Role == "Father", "black", NA)


plot(moneyNetwork)


  
attributes = rbind(attributes, attributes[2,])
attributes[6,]$Name = "Bobina"

in_network = which(attributes$Name %in% unique(c(money_edgelist$Ego, money_edgelist$Alter)))

attributes = attributes[in_network,,]

install.packages("car")
library(car)

E(moneyNetwork)$frequency = c(10, 1, 1, 40, 30, 1, 1, 50, 35, 5, 3)

