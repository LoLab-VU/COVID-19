# CCLE ACE2 expression in cell lines
d <- read.csv("mRNA expression (RNAseq)_ ACE2.txt", sep='\t', as.is=TRUE)

v <- as.numeric(d[,-1])
names(v) <- colnames(d[,-1])
v <- v[order(v, decreasing=TRUE)]
head(v,50)
