library(pheatmap)
library(gplots)
library(RColorBrewer)

################
## Load model ##
################

source("/Users/ricard/MOFA_microbiome/ricard/load_model.R")

io$outdir <- "/Users/ricard/data/mofa_microbiome/pdf/mofa_overview"

#####################
## Define settings ##
#####################

opts$palettes <- list(
  "Bacteria" = colorRampPalette(rev(brewer.pal(n = 7, name="RdYlBu")))(100),
  "Fungi" = colorRampPalette(c("darkgreen", "red"))(100),
  "Viruses" = colorRampPalette(c("red","white","blue"))(100)
)

###################
## Plot heatmaps ##
###################

## Bacteria ##
Y <- mofa@data$Bacteria$single_group

pdf(sprintf("%s/Y_bacteria.pdf",io$outdir), height=3.5, width=3.5)
pheatmap(Y, 
  cluster_rows = F, cluster_cols = F,
  border_color = "black", legend = FALSE, 
  color = opts$palettes[["Bacteria"]],
  show_rownames = F, show_colnames = F
)
dev.off()


## Fungi ##
Y <- mofa@data$Fungi$single_group

pdf(sprintf("%s/Y_fungi.pdf",io$outdir), height=3.5, width=3.5)
pheatmap(Y, 
 cluster_rows = F, cluster_cols = F,
 border_color = "black", legend = FALSE, 
 color = opts$palettes[["Fungi"]],
 show_rownames = F, show_colnames = F
)
dev.off()

## Viruses ##
Y <- mofa@data$Viruses$single_group

pdf(sprintf("%s/Y_viruses.pdf",io$outdir), height=3.5, width=3.5)
pheatmap(Y, 
 cluster_rows = F, cluster_cols = F,
 border_color = "black", legend = FALSE, 
 color = opts$palettes[["Viruses"]],
 show_rownames = F, show_colnames = F
)
dev.off()