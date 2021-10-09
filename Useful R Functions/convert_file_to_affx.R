genotype = read.csv("~/gh_repos/neanderthal/genotype_map_larger.csv", stringsAsFactors = F, header = T)
files_to_fix = paste0("~/gh_repos/neanderthal/gwas_summary_stats/",list.files("~/gh_repos/neanderthal/gwas_summary_stats"))

for(i in files_to_fix){
  sum_stats = read.table(i, fill = T, stringsAsFactors = F, header = T)
  original_max = ncol(sum_stats)
  sum_stats[,1] = ifelse(sum_stats[,1] %in% genotype$dbSNP.RS.ID, genotype$Affy.SNP.ID[match(sum_stats[,1], genotype$dbSNP.RS.ID)], NA)
  sum_stats = sum_stats[is.na(sum_stats[,1]) == F,]
  new_name = strsplit(i, "\\.")[[1]][1]
  write.table(sum_stats, paste0(new_name, "_Fixed.txt"), quote = F, sep = "\t")
  rm(list = "sum_stats")
}

neanderthal = read.table("~/Desktop/neanderthal_snps_list.txt", header = T)
neanderthal$id = gsub("[^0-9]","",neanderthal$snp)

neanderthal$new_id = ifelse(neanderthal$id %in% genotype$V4, genotype$V3[match(neanderthal$id, genotype$V4)], NA)
neanderthal = na.omit(neanderthal)
neanderthal$new_id = paste0("Affx-", neanderthal$new_id)

write.table(neanderthal$new_id, "neanderthal_affx_snp.list", row.names = F, col.names = F, quote = F)


neanderthal$snp = neanderthal$new_id
neanderthal$weight = 1
neanderthal = subset(neanderthal, select = c(snp, derived_auto, weight))

write.table(neanderthal, "neanderthal_affx_snp.txt", row.names = F, col.names = T, quote = F, sep = "\t")

rm(list = c("genotype", "neanderthal"))





x = read.table("/Users/markhoffman/Desktop/gcta_old/x.bim")
neanderthal = read.table("~/Desktop/neanderthal_snps_list.txt", header = T)
neanderthal_sub = neanderthal[which(neanderthal$snp %in% x$V2),]



bed = "/Users/markhoffman/Desktop/gcta_old/x.bed"
bim = "/Users/markhoffman/Desktop/gcta_old/x.bim"
fam = "/Users/markhoffman/Desktop/gcta_old/x.fam"
dat = read.plink(bed, bim, fam)
dat = dat$genotypes[,as.character(neanderthal_sub$snp)]
dat = as.matrix(dat)
dimensions = dim(dat)
data = matrix(sapply(dat, as.numeric), nrow = dimensions[1], ncol = dimensions[2])
colnames(data) = colnames(dat)
data[] = ifelse(data[] == 0, NA, data[])
data[] = ifelse(data[] == 3, 0, data[])
data[] = ifelse(data[] %in% c(1,2), 1, data[])
poly_score = rowMeans(data, na.rm = T)

save_props = c()
for(i in 1:ncol(data)){
  save_props = c(save_props, as.numeric(1-prop.table(table(data[,i]))["3"]))
}




