hierarchy = read.csv("hierarchical_structure.csv", header = F)
hierachy_net = graph.adjacency(as.matrix(hierarchy), mode = "undirected")
V(hierachy_net)$name = as.character(1:18)

# pure
plot(hierachy_net, layout = layout_with_fr, vertex.color = "white", vertex.label.color = "black", vertex.frame.color = "grey60")
plot(hierachy_net, layout = layout_with_kk, vertex.color = "white", vertex.label.color = "black", vertex.frame.color = "grey60")
plot(hierachy_net, layout = layout_with_graphopt, vertex.color = "white", vertex.label.color = "black", vertex.frame.color = "grey60")

# messy
plot(hierachy_net, layout = layout_with_gem, vertex.color = "white", vertex.label.color = "black", vertex.frame.color = "grey60")
plot(hierachy_net, layout = layout_with_dh, vertex.color = "white", vertex.label.color = "black", vertex.frame.color = "grey60")
plot(hierachy_net, layout = layout_with_drl, vertex.color = "white", vertex.label.color = "black", vertex.frame.color = "grey60")

# structural equivalence
plot(hierachy_net, layout = layout_with_mds, vertex.color = "white", vertex.label.color = "black", vertex.frame.color = "grey60")

