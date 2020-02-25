source("/Users/ricard/MOFA_microbiome/ricard/load_settings.R")
source("/Users/ricard/MOFA_microbiome/ricard/load_model.R")

########################
## Plot factor values ##
########################

p <- plot_factors(mofa, c(1,4), color_by = "Category", shape_by="Seks", dot_size = 3) +
  # scale_fill_brewer(palette = "Set2") +
  # scale_fill_manual(values=opts$colors) +
  theme(
    legend.title = element_blank(),
  )

pdf(sprintf("%s/Factor1_vs_Factor4_category.pdf",io$outdir), width=7, height=5, useDingbats = F)
print(p)
dev.off()

p <- plot_factor(mofa, 
  factor = 4, 
  color_by = "Category", 
  dot_size = 4,
  dodge = TRUE, scale = TRUE,
  stroke = 0.4,
  add_violin = T, color_violin = T
) +
  scale_fill_manual(values=opts$colors) +
  scale_color_manual(values=opts$colors) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

pdf(sprintf("%s/Factor4_category.pdf",io$outdir), width=2.5, height=5, useDingbats = F)
print(p)
dev.off()


pdf(sprintf("%s/Factor4_Beta_lactam.pdf",io$outdir), width=5, height=5, useDingbats = F)
plot_factor(mofa, factors = 4, color_by = "Beta_lactam", dot_size = 3, add_violin = TRUE, dodge=TRUE, legend = TRUE)
dev.off()

pdf(sprintf("%s/Factor4_Cephalosporins.pdf",io$outdir), width=5, height=5, useDingbats = F)
plot_factor(mofa, factors = 4, color_by = "Cephalosporins", dot_size = 3, add_violin = TRUE, dodge=TRUE, legend = TRUE)
dev.off()


##################
## Plot weights ##
##################

## Bacteria ##

pdf(sprintf("%s/Factor4_bacteria_weights.pdf",io$outdir), width=4, height=5.5, useDingbats = F)
plot_weights(mofa, 
  factors = 4, 
  view = "Bacteria", 
  nfeatures = 10,
  # manual = list("A"=opts$good.bacteria, "B"=opts$bad.bacteria),
  color_manual = c("black","black"),
  text_size = 5
)
dev.off()


opts$fungi <- c("Aspergillus","Penicillium")

pdf(sprintf("%s/Factor4_fungi_weights.pdf",io$outdir), width=4, height=5.5, useDingbats = F)
plot_weights(mofa, 
  factors = 4, 
  view = "Fungi", 
  manual = list(opts$fungi),
  text_size = 5
)
dev.off()


###################
## Plot heatmaps ##
###################

## Bacteria ##

pdf(sprintf("%s/Factor4_bacteria_heatmap.pdf",io$outdir), width=7, height=5)
plot_data_heatmap(mofa, 
  factor = 4, 
  view = "Bacteria", 
  # features = c(good.bacteria,bad.bacteria),
  # color = viridis(100),
  denoise = TRUE, 
  legend = FALSE,
  cluster_rows = T, cluster_cols = F,
  show_colnames = F, show_rownames = T,
  # annotation_samples = "Category",  annotation_colors = list("Category"=opts$colors), annotation_legend = F,
  annotation_samples = "Cephalosporins", annotation_legend = F,
  scale = "row"
)
dev.off()


## Fungi ##


pdf(sprintf("%s/Factor4_fungi_heatmap.pdf",io$outdir), width=7, height=5)
plot_data_heatmap(mofa, 
  factor = 4, 
  view = "Fungi", 
  # features = c(opts$good.fungi,opts$bad.fungi),
  # color = viridis(100),
  denoise = TRUE, 
  legend = FALSE,
  cluster_rows = T, cluster_cols = F,
  show_colnames = F, show_rownames = T,
  annotation_samples = "Category",  annotation_colors = list("Category"=opts$colors), annotation_legend = F,
  scale = "row"
)
dev.off()


#########################################
## Plot factor values coloured by OTUs ##
#########################################

# plot factor values coloured by features with top weights
# sign <- "negative"
sign <- "positive"
nfeatures <- 4
nrow <- 2; ncol <- 2

for (i in views(mofa)) {
  
  weights <- sort(get_weights(mofa, factor=1, views=i)[[1]][,1])
  if (sign=="positive") {
    features <- names( tail(weights,n=nfeatures) )
  } else {
    features <- names( head(weights,n=nfeatures) )
  }
  
  p_list <- list()
  for (j in features) {
    p_list[[j]] <- plot_factors(mofa, c(1,3), color_by = j, dot_size = 3.0, legend=FALSE) +
      ggtitle(j) +
      theme(
        plot.title = element_text(hjust = 0.5, size=rel(1.2)),
        axis.title = element_text(size=rel(0.8)),
        axis.text = element_text(size=rel(0.8))
      )
  }
  p <- cowplot::plot_grid(plotlist=p_list, nrow=nrow, ncol=ncol)
  
  pdf(sprintf("%s/Factor4_vs_Factor3_%s_%s.pdf",io$outdir,i,sign), width=8, height=7, useDingbats = F)
  print(p)
  dev.off()
    
}
