assign_atts <- function(g, df){
  require(igraph)
  var_names <- colnames(subset(df, select = c(-name)))
  temp_index <- match(V(g)$name, df$name)
  for ( i in 1:length(var_names)){
   g <- set.vertex.attribute(g, var_names[i], index = temp_index, df[temp_index,var_names[i]])
  }
  return(g)
}