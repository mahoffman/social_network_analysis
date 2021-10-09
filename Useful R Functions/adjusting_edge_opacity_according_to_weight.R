# Adjusting edge.opacity according to edge weight #
# This relies on the net59 network we made in tutorial 2 with attributes matched in.

# First we have to adjust rank so that it goes from 0 to 1 as well as reverse it so that 
# stronger rankings have higher values. 

# Reverse edge ranks (so that highest rank has highest value)
E(net59)$rev_rank <- (max(E(net59)$rank)-E(net59)$rank)+1

# Normalize rank to go from 1/max to 1
E(net59)$norm_rev_rank <- E(net59)$rev_rank/max(E(net59)$rev_rank)

# Color edges uniformly
E(net59)$color <- "grey70"

# Adjust color by changing the alpha (opacity) in accordance with the normalized, reverse rank 
# (i.e. the weight variable)
# Sadly, adjustcolor is not vectorized in the way that we would want it to be so we have to use a loop
for(i in unique(E(net59)$norm_rev_rank)){
  rank_ids <- E(net59)$norm_rev_rank == i
  E(net59)$color[rank_ids] <- adjustcolor(E(net59)$color[rank_ids], alpha.f = i)
}

# Plot the result
plot(net59)

# We can adjust edge widths too
E(net59)$width <- E(net59)$norm_rev_rank

plot(net59) # looks cool!