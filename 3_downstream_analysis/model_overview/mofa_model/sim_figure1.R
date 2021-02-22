library(RColorBrewer)
library(gplots)
library(pheatmap)

#####################
## Define settings ##
#####################

N <- 59
D <- c(180,18,42)
K <- 6

outdir <- "/Users/ricard/data/mofa_microbiome/pdf/mofa_overview"

############################
## Simulate factor matrix ##
############################

pdf(paste0(outdir,"/Z.pdf"), height=3, width=6)
Z <- matrix(rnorm(N*K,0,5),N,K)
my_palette <- colorRampPalette(c("white", "darkgrey"))(n=3)
# pheatmap::pheatmap(Z, cluster_rows=F, cluster_cols=F, border_color="black", legend=FALSE, color=my_palette, cellheight=6, cellwidth=5.5)
heatmap.2(t(Z), col = my_palette, na.color = "gray95", dendogram="none", Rowv=F, Colv=F, density.info="none", trace="none",
          key=FALSE, labRow = FALSE, labCol = FALSE, sepwidth=c(0.01,0.01), sepcolor="black", colsep=0:nrow(Z), rowsep=0:ncol(Z))
dev.off()

##############################
## Simulate data modalities ##
##############################

missing_samples <- 1
missing_values <- 0.1

Y <- matrix(sample(1:5,size=N*D,replace=T),N,D)
Y[sample(N*D, N*D*missing_values, replace = F)] <- NA
Y[sample(N, missing_samples, replace = F),] <- NA

my_palette = colorRampPalette(rev(brewer.pal(n = 7, name="RdYlBu")))(100)
my_palette <- colorRampPalette(c("darkgreen", "red"))(n=2)
my_palette <- colorRampPalette(c("red","white","blue"))(256)

pdf(sprintf("%s/Y3.pdf",outdir), height=2.5, width=9)
pheatmap::pheatmap(t(Y), cluster_rows=F, cluster_cols=F, border_color="black", legend=FALSE, color=my_palette)
# heatmap.2(t(Y), col = my_palette, na.color = "gray95", dendogram="none", Rowv=F, Colv=F, density.info="none", trace="none",
#           key=FALSE, labRow = FALSE, labCol = FALSE, sepwidth=c(0.00,0.00), sepcolor="white", colsep=0:nrow(Y), rowsep=0:ncol(Y))
dev.off()


##############################
## Simulate weight matrices ##
##############################

# W <- abs(read.table(sprintf("%s/W3.txt",outdir))) %>% as.matrix

my_palette <- colorRampPalette(c("white", "red"))(n=5)
pdf(sprintf("%s/W3.pdf",outdir), height=5, width=3)
pheatmap::pheatmap(W, cluster_rows=F, cluster_cols=F, border_color="black", legend=FALSE, color=my_palette, show_rownames = F, show_colnames = F)
# heatmap.2(W1, col = my_palette, na.color = "gray95", dendogram="none", Rowv=F, Colv=F, density.info="none", trace="none",
#           key=FALSE, labRow = FALSE, labCol = FALSE, sepwidth=c(0.01,0.01), sepcolor="black", colsep=0:nrow(W1), rowsep=0:ncol(W1))
dev.off()

