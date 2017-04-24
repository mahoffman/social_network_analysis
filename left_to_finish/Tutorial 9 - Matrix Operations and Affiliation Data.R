# ******************************************************************************************************************
# AFFILIATION DATA
# ******************************************************************************************************************

# This portion of the tutorial focuses on affiliation data.  
# Individuals can be directly linked to one another by affections or interactions. We have spent most
# of the class so far detailing and analyzing the various types of direct relations. 

# However, they can also be linked through "affiliations", that is, shared associations to groups or objects.  

# As an example, people might be tied by the classes they have taken together.
# The data might look like:

# Person,  Classes
# Leo,     Biostatistics, Chemistry, Linear Algebra
# Clement, Islamic Civilization, The Modern World-System, Exile and Diaspora
# Palla,   Calc 1, Calc 2, Linear Algebra, 
# Filippo, Linear Algebra, Social Networks, The Modern World-System

# We can create a network with two types of nodes - one set of nodes will be people, the other classes.
# People, in this network, cannot be directly tied to each other. Rather they are co-affiliated with a 
# class, which serves as the basis of their connection. 
# Therefore, all ties will be between nodes of different types.

# To create this network, we need to turn the above data into an edgelist, convert it to a matrix, 
# and plot it in igraph.
library(igraph)
# Let's start with the data.

classes_data <- data.frame(name = c("Leo", "Clement", "Palla", "Filippo"),
                           class1 = c("Biostatistics","Islamic Civ", "Calc 1", "Linear Algebra"),
                           class2 = c("Chemistry", "The Modern World-System", "Calc 2", "Social Networks"),
                           class3 = c("Linear Algebra", "Exile and Diaspora", "Linear Algebra", "The Modern World-System"),
                           stringsAsFactors = FALSE)

View(classes_data)

# The reshape packages will let us convert the classes_data to an edgelist

# install.packages("reshape2")
library(reshape2)
classes_data <- melt(classes_data, measure.vars = c("class1", "class2","class3"), value.name = "classes", variable.name = "order")
# The ?melt function turns short form data into long form. It takes the class# variables and combines
# them into a single variable called classes.

classes_data <- subset(classes_data, select = c("name", "classes")) # We only want two columns, so we select the two we want using subset.

classesMatrix = table(classes_data) # We can then use the table function to turn it into an incidence matrix

class(classesMatrix) <- "matrix" # And we convert it from a table to a matrix
View(classesMatrix) # Cool!

# In an incidence matrix, the rows are  one class of node, while columns are another.
# The rows are generally people who are affiliated with groups in the columns. 

# Using the get.incidence() function will turn our matrix into a bipartite network. 
classesNet <- graph.incidence(classesMatrix, mode = c("all"))
plot(classesNet)

# We can change the shape of nodes to highlight their type.
V(classesNet)$shape <- ifelse(V(classesNet)$type == FALSE, "circle", "square")
plot(classesNet)

# ******************************************************************************************************************
# Unipartite Projection: Dot product
# ******************************************************************************************************************

# Bipartite networks can be represented (or "projected") as unipartite networks.  In this case, either people will be the
# only nodes, and they will be connected if they share an affiliation (i.e. they are in the same group) OR groups will 
# be the only nodes and they will be connected if they share an affiliation to a person. 

# We can make the projection two ways - using the bipartite.projection() function in igraph, or by
# multiplying the incidence matrix by its transpose (or vise versa).  

# The mathematical operation to make a person-to-person projection is to multiply the initial matrix by its transpose. 

# In R that looks like:
personMatrix = classesMatrix %*% t(classesMatrix) # the t() function transposes the matrix that is passed to it

diag(personMatrix) <- 0 # the diagonal tells us the number of groups each person is affiliated with, 
                        # but we set it to 0 using the ?diag function
View(personMatrix)

# To get the group-to-group matrix, we multiply the transpose by the initial matrix (reverse!)
groupMatrix = t(classesMatrix) %*% classesMatrix
View(groupMatrix) # The diagonal details the number of people in each class

diag(groupMatrix) <- 0 # we again set it to 0

# Both of these operations turn our rectangular incidence matrix into a square adjacency matrix. Order matters.
# Now that we have adjacency matrices can use the graph.adjacency() function to turn them into network objects.
personNet <- graph.adjacency(personMatrix, mode = "undirected")
groupNet <- graph.adjacency(groupMatrix, mode = "undirected")

plot(personNet)
plot(groupNet)

# We can analyze these networks just like we would any other network with a single node type.

# ******************************************************************************************************************
# Detecting groups
# ******************************************************************************************************************

# Finally, we can also detect groups in a network using a community detection algorithm. The best one for undirected graphs is "multilevel.community". For direct graphs, you can use walktrap.community.

groups_of_classes = multilevel.community(groupNet)

# The communities function extracts the communities, while the membership function returns a vector designating for each actor the community they belong to.

communities(groups_of_classes)
membership(groups_of_classes)

# We can mark groups in our plots in two ways. 
# The first way is to assign the mark.groups argument in plot to be equal to communities(groups_of_classes); it will draw shapes around the groups to designate them. This works well in small networks.
plot(groupNet, mark.groups = communities(groups_of_classes))

# In larger networks it is often better to just assign node colors according group membership. 
V(groupNet)$color = c("Green", "Blue")[membership(groups_of_classes)]
plot(groupNet, vertex.label.color = "black")