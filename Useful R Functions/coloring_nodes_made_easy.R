#### How to color node in R by an attribute
# STRATEGY 1: ifelse() statements; or the worst strategy and sadly the only one I mentioned in class #
V(net59)$color <- NA
V(net59)$color <- ifelse(V(net59)$grade == 7, 
                         "red", 
                         V(net59)$color)

V(net59)$color <- ifelse(V(net59)$grade == 8, 
                         "blue", 
                         V(net59)$color)

V(net59)$color <- ifelse(V(net59)$grade == 9, 
                         "orange", 
                         V(net59)$color)

V(net59)$color <- ifelse(V(net59)$grade == 10, 
                         "pink", 
                         V(net59)$color)

V(net59)$color <- ifelse(V(net59)$grade == 11, 
                         "cyan", 
                         V(net59)$color)

V(net59)$color <- ifelse(V(net59)$grade == 12, 
                         "yellow", 
                         V(net59)$color)

#plot
plot(net59)

# STRATEGY 2: Use straightforward indexing and a color scheme we defined ourselves to color by grade #
V(net59)$color <- NA
V(net59)$color[V(net59)$grade == 7] <- "red" 
V(net59)$color[V(net59)$grade == 8] <- "blue" 
V(net59)$color[V(net59)$grade == 9] <- "orange" 
V(net59)$color[V(net59)$grade == 10] <- "pink" 
V(net59)$color[V(net59)$grade == 11] <- "cyan" 
V(net59)$color[V(net59)$grade == 12] <- "yellow" 

#plot
plot(net59)

# Strategy 3: Using RColorBrewer and a useful attribute of factors
library(RColorBrewer)

# set the number of colors we need as the number of unique races in the data
num_colors <- length(unique(V(net59)$grade))

# now we will use RColorBrewer's brewer.pal() function to generate N (here the number of races) colors 
# from the a given color palette. 
# Let's take a look at the Dark2 palette
display.brewer.pal(n = num_colors, name = 'Dark2')

# looks good! let's use it as our palette.
pal <- brewer.pal(num_colors, "Dark2")

# when a vector is converted to a factor and then back to a numeric, the resulting numbers will  
# start at 1 and go up to N (the number of unique categories in the data)
# we can exploit that to easily index the palette above
V(net59)$color <- pal[as.numeric(as.factor(vertex_attr(net59, "grade")))]

#plot
plot(net59)

# STRATEGY 4: Turn Strategy 3 into a function and use it to color nodes

## Instead of using the code for strategy 3 over and over again, 
# we could write our own function in R to do this.
color_nodes_by_attribute <- function(net, attribute, color_scheme = "Dark2"){
  
  # load in necessary packages
  require(igraph)
  require(RColorBrewer)
  
  # grab the attribute values using the igraph function vertex_attr()
  vertex_att <- vertex_attr(net, attribute)
  
  # evaluate the number of unique attribute values
  num_colors <- length(unique(vertex_att))
  
  # use RColorBrewer to create the color palette
  pal <- brewer.pal(num_colors, color_scheme)
  
  # make the node colors
  node_colors <- pal[as.numeric(as.factor(vertex_attr(net, attribute)))]
  
  # return the node colors
  return(node_colors)
}

# Now we can simply use this function and pass to it:
# the network, the attribute we want to use, and a RColorBrewer color scheme.
# It will return colors for each node. 
# We assign the result to the color attribute in the network. 
# Here you can also see that I changed the color_scheme to Accent
V(net59)$color <- color_nodes_by_attribute(net59, "grade", color_scheme = "Accent")

# plot
plot(net59)
