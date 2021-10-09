# Original algorithm by Ronald L. Breiger, Scott A. Boorman and Phipps Arabie (1975)
# Code from Adam Slez, Department of Sociology, University of Virginia

is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 

igraph_to_matrices <- function(net_list){
  igraphs <- which(unlist(lapply(net_list, class)) == "igraph")
  for ( i in igraphs){
    net_list[[i]] <- as.matrix(get.adjacency(net_list[[i]]))
  }
  return(net_list)
}

concor_hca <- function(m0, cutoff = 0.999, max.iter = 25, p = 1) {
  mat_stack <- do.call(rbind, m0)
  p_list <- list(mat_stack)
  for(i in 1:p) {
    p_list <- unlist(lapply(p_list, 
                            function(x) concor(x, cutoff, max.iter, time = i)), 
                     recursive = FALSE)
  }
  df <- do.call(rbind, block_names(p_list))
  df[match(rownames(m0[[1]]), df$vertex), ]
}

concor <- function(mat_stack, cutoff = 0.999, max.iter = 50, time = 1) {
  require(corrplot)
  if (ncol(mat_stack) < 2) stop("Too few columns to partition.")
  mi <- cor(mat_stack)
  iter <- 1
  while(any(abs(mi) <= cutoff) & iter <= max.iter) {
    mi <- cor(mi)
    iter <- iter + 1
  }
  if(time == 1) corrplot(mi)
  group <- mi[, 1] > 0
  list(mat_stack[, group, drop = FALSE], mat_stack[, !group, drop = FALSE])
}

block_names <- function(p_list) {
  lapply(seq_along(p_list), 
         function(x) data.frame(block = x, 
                                vertex = colnames(p_list[[x]]),
                                stringsAsFactors = FALSE))
}

get_blocks <- function(block_model, num_relat){
  block_model <- as.numeric(block_model)
  block_list <- list()
  for ( i in 1:num_relat){
    id <- seq(from = 1, to = length(block_model), by = num_relat) + (i-1)
    temp_block <- block_model[id]
    temp_mat <- matrix(temp_block, nrow = sqrt(length(temp_block)), byrow = T)
    colnames(temp_mat) <- paste("Block",1:ncol(temp_mat))
    rownames(temp_mat) <- paste("Block",1:ncol(temp_mat))
    block_list[[length(block_list) + 1]] <- temp_mat
  }
  return(block_list)
}

concor_blockmodel <- function(relation_list, cutoff = 0.999, max.iter = 25, p = 1){
  require(igraph)
  if(all(unlist(lapply(relation_list, class)) %in% c("igraph", "matrix")) == FALSE){
    stop("One of the objects in your list is not a matrix or an igraph object!")
  }
  if(any(unlist(lapply(relation_list, class)) == "igraph")){
    relation_list <- igraph_to_matrices(relation_list)
  }
  if(is.null(names(relation_list))){
    names(relation_list) <- 1:length(relation_list)
  }
  if(is.installed("sna") == FALSE){
    stop("You need to install the package 'sna' to proceed.")
  }
  concor_out <- concor_hca(relation_list, cutoff, max.iter, p)
  blocker_out <- sna::blockmodel(relation_list, concor_out$block, 
                            glabels = names(relation_list),
                            plabels = rownames(relation_list[[1]]))
  par(mar = c(1,0,1,0))
  sna::plot.blockmodel(blocker_out)
  blocks <- blocker_out$block.model
  blocks <- get_blocks(blocks, num_relat = length(relation_list))
  return(list(Membership = blocker_out$block.membership, 
              Order =  blocker_out$order.vector,
              Block.Model = blocks))
}