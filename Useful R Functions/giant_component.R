giant_component <- function(graph, ...) {
  cl <- clusters(graph, ...)
  induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
}