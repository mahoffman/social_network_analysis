edge_color_by_node_color <- function(graph){
  require(igraph)
  if ( is.null(V(graph)$color)==TRUE ) warning("Vertices must be colored.")
  
  for (i in E(graph)){
    
    head <- head_of(graph, E(graph)[i])
    tail <- tail_of(graph, E(graph)[i])
    head_color <- V(graph)[head]$color
    tail_color <- V(graph)[tail]$color
    colfunc <- colorRampPalette(c(head_color, tail_color))
    gradient <- colfunc(100)
    
    E(graph)[i]$color <- gradient[50]
  }
  return(graph)
}