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

write.csv(marriage_edges, "florentine_marriage_edgelist.csv")

partner_data = readLines("partner116.txt")

partner_edges = partner_data[119:length(partner_data)]
partner_edges = strsplit(partner_edges, " ")
library(data.table)
partner_edges = rbindlist(lapply(partner_edges, FUN = function(x) as.data.frame(t(as.matrix(subset(x, x != ""))), stringsAsFactors = F)))

partner_edges$V1 =  family_ids[match(partner_edges$V1, 1:length(family_ids))]
partner_edges$V2 =  family_ids[match(partner_edges$V2, 1:length(family_ids))]
partner_edges = partner_edges[,1:2]
colnames(partner_edges) = c("FamilyA", "FamilyB")

write.csv(partner_edges, "florentine_partner_edgelist.csv")


credit_data = readLines("credit116.txt")

credit_edges = credit_data[119:length(credit_data)]
credit_edges = strsplit(credit_edges, " ")
library(data.table)
credit_edges = rbindlist(lapply(credit_edges, FUN = function(x) as.data.frame(t(as.matrix(subset(x, x != ""))), stringsAsFactors = F)))

credit_edges$V1 =  family_ids[match(credit_edges$V1, 1:length(family_ids))]
credit_edges$V2 =  family_ids[match(credit_edges$V2, 1:length(family_ids))]
credit_edges = credit_edges[,1:2]
colnames(credit_edges) = c("FamilyA", "FamilyB")

write.csv(credit_edges, "florentine_credit_edgelist.csv")

atts = read.csv("Padget116attributes.csv")
atts$Family = family_ids
write.csv(atts, "florentine_attributes.csv", row.names = F)