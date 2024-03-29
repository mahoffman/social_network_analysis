---
title: "Network Data Introduction"
author: "Mark Hoffman"
output: html_document
---

# Understanding network data structures

Underlying every network visualization is data about relationships. These relationships can be observed or simulated (that is, hypothetical). When analyzing a set of relationships, we will generally use one of two different data structures: edge lists or adjacency matrices.

## Edge lists

One simple way to represent a graph is to list the edges, which we will refer to as an edge list. For each edge, we just list who that edge is incident on. Edge lists are therefore two column matrices that directly tell the computer which actors are tied for each edge. In a directed graph, the actors in column A are the sources of edges, and the actors in Column B receive the tie. In an undirected graph, order doesn't matter.

In R, we can create an example edge list using vectors and data.frames.  I specify each column of the edge list with vectors and then assign them as the columns of a data.frame. We can use this to visualize what an edge list should look like. 


```r
personA <- c("Mark", "Mark", "Peter", "Peter", "Bob", "Jill")
personB <- c("Peter", "Jill", "Bob", "Aaron", "Jill", "Aaron")

edgelist <- data.frame(PersonA = personA, PersonB = personB, stringsAsFactors = F)

print(edgelist)
```

```
##   PersonA PersonB
## 1    Mark   Peter
## 2    Mark    Jill
## 3   Peter     Bob
## 4   Peter   Aaron
## 5     Bob    Jill
## 6    Jill   Aaron
```

What are the upsides of using the edge list format? As you can see, in an edge list, the number of rows accords to the number of edges in the network since each row details the actors in a specific tie. It is therefore really simple format for _recording_ network data in an excel file or csv file. 

What are the downsides? The first is practical - it is impossible to represent isolates using an edge list since it details relations. There are ways to get around this problem in R, however. The second is technical - edge lists are not suitable for data formats for performing linear algebraic techniques. As a result, we will almost always convert and edge list into either an adjacency matrix, or into a network object. 

## Adjacency matrices

Adjacency matrices have one row and one column for each actor in the network.  The elements of the matrix can be any number but in most networks, will be either 0 or 1. A matrix element of 1 (or greater) signals that the respective column actor and row actor should be tied in the network. Zero signals that they are not tied. 

We can use what we learned in the last tutorial to create such a matrix. An example might look like this:

```r
adjacency <- matrix(c(0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0), nrow = 5, ncol = 5, dimnames = list(c("Mark", "Peter", "Bob", "Jill", "Aaron"), c("Mark", "Peter", "Bob", "Jill", "Aaron")))

print(adjacency)
```

```
##       Mark Peter Bob Jill Aaron
## Mark     0     1   0    1     0
## Peter    1     0   1    0     1
## Bob      0     1   0    1     0
## Jill     1     0   1    0     1
## Aaron    0     1   0    1     0
```

What are the upsides of using the adjacency matrix format? Adjacency matrices are the most fundamental network analysis data format.  They undergird all of the analytical techniques we will show you later on. They are also much more efficient than edge lists. For example, imagine searching for whether Peter and Jill are friends in an adjacency matrix as opposed to an edge list.  In the adjacency matrix, we would go to Peter's row and Jill's column and we would find either a 1 or a 0, giving us our answer. In an edge list, we would have to search through each edge, which might seem simple in a dataset with only 5 people, but as the edge list grows, it will scale linearly with the number of edges.

What are the downsides? It is really difficult to record network data with an adjacency matrix. They are better suited for computers than people.  
