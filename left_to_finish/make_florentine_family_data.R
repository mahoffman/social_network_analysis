marriage_data = readLines("marriage116.txt")
family_ids = marriage_data[2:117]
family_ids = unlist(lapply(strsplit(family_ids, "\""), FUN = function(x) x[2]))

marriage_edges = marriage_data[119:length(marriage_data)]
marriage_edges = strsplit(marriage_edges, " ")
library(data.table)
marriage_edges = rbindlist(lapply(marriage_edges, FUN = function(x) as.data.frame(t(as.matrix(subset(x, x != ""))), stringsAsFactors = F)))

marriage_edges$V1 =  family_ids[match(marriage_edges$V1, 1:length(family_ids))]
marriage_edges$V2 =  family_ids[match(marriage_edges$V2, 1:length(family_ids))]
marriage_edges = marriage_edges[,1:2]
colnames(marriage_edges) = c("FamilyA", "FamilyB")

write.csv(marriage_edges, "florentine_edgelist.csv")

atts = read.csv("Padget116attributes.csv")
atts$Family = family_ids
write.csv(atts, "florentine_attributes.csv", row.names = F)