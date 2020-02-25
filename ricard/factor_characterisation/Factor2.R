source("/Users/ricard/MOFA_microbiome/ricard/load_settings.R")
source("/Users/ricard/MOFA_microbiome/ricard/load_model.R")

########################
## Plot factor values ##
########################

p <- plot_factor(mofa, 
  factor = 2, 
  color_by = "Category", 
  dot_size = 4,
  dodge = TRUE,
  stroke = 0.4,
  add_violin = T, color_violin = T
) +
  scale_fill_manual(values=opts$colors) +
  scale_color_manual(values=opts$colors) +
  theme(
    legend.position = "right",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

pdf(sprintf("%s/Factor2_category.pdf",io$outdir), width=2.5, height=5, useDingbats = F)
print(p)
dev.off()

##################
## Plot weights ##
##################

pdf(sprintf("%s/Factor2_fungi_weights.pdf",io$outdir), width=4, height=5.5, useDingbats = F)
plot_weights(mofa, 
  factors = 2, 
  view = "Fungi", 
  # manual = list("A"=opts$good.fungi, "B"=opts$bad.fungi),
  color_manual = c("black","black"),
  text_size = 5
)
dev.off()

pdf(sprintf("%s/Factor2_fungi_top_weights.pdf",io$outdir), width=4, height=5.5, useDingbats = F)
plot_top_weights(mofa, 
  factors = 2, 
  view="Fungi", 
  sign = "all", 
  abs=F, 
  nfeatures = 15
)
dev.off()

###################
## Plot heatmaps ##
###################

pdf(sprintf("%s/Factor2_fungi_heatmap.pdf",io$outdir), width=7, height=5)
plot_data_heatmap(mofa, 
  factor = 2, 
  view = "Fungi", 
  # features = c(opts$good.fungi,opts$bad.fungi),
  # color = viridis(100),
  denoise = TRUE, 
  legend = TRUE,
  cluster_rows = T, cluster_cols = F,
  show_colnames = F, show_rownames = T,
  annotation_samples = "Category",  annotation_colors = list("Category"=opts$colors), annotation_legend = F,
  scale = "row"
)
dev.off()


#########################################
## Plot factor values coloured by OTUs ##
#########################################

sign <- "negative"
# sign <- "positive"
nfeatures <- 4
nrow <- 2; ncol <- 2

# Fetch features with the largest weight  
weights <- sort(get_weights(mofa, factor=1, views="Fungi")[[1]][,1])
if (sign=="positive") {
  features <- names( tail(weights,n=nfeatures) )
} else if (sign=="negative") {
  features <- names( head(weights,n=nfeatures) )
}

p_list <- list()
for (j in features) {
  p_list[[j]] <- plot_factor(mofa, 
      factor = 2, 
      color_by = j, shape_by="Category",
      dot_size = 3.5, stroke = 0.4,
      legend = FALSE,
      # add_violin = T, color_violin = T
      dodge = TRUE
    ) +
    scale_fill_gradient(low = "white", high = "red") +
    ggtitle(j) +
    theme(
      plot.title = element_text(hjust = 0.5, size=rel(1.1)),
      axis.title = element_text(size=rel(1.0)),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size=rel(0.8)),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_line(size=rel(0.8))
    )
}
p <- cowplot::plot_grid(plotlist=p_list, nrow=nrow, ncol=ncol)
  
pdf(sprintf("%s/Factor2_expr_%s.pdf",io$outdir,sign), width=6, height=7, useDingbats = F)
print(p)
dev.off()
    

#######################
## Plot data scatter ##
#######################

# sign <- "negative"
sign <- "positive"

nfeatures <- 4

p <- plot_data_scatter(mofa,
  factor = 2, view = "Fungi",
  dot_size = 2,
  features = nfeatures,
  color_by = "Category",
  sign = sign
) 
p <- p + scale_color_manual(values=opts$colors) +
  theme(
    axis.title = element_text(size=rel(1.0)),
    axis.text = element_text(size=rel(0.8))
  )

pdf(sprintf("%s/Factor2_scatter_expr_%s.pdf",io$outdir,sign), width=8, height=7, useDingbats = F)
print(p)
dev.off()
