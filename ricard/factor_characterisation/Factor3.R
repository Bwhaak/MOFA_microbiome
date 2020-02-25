source("/Users/ricard/MOFA_microbiome/ricard/load_settings.R")
source("/Users/ricard/MOFA_microbiome/ricard/load_model.R")

########################
## Plot factor values ##
########################

p <- plot_factor(mofa, 
  factor = 3, 
  color_by = "Category", 
  dot_size = 4,
  dodge = TRUE,
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

pdf(sprintf("%s/Factor3_category.pdf",io$outdir), width=2.5, height=5, useDingbats = F)
print(p)
dev.off()

##################
## Plot weights ##
##################

## Bacteria ##

pdf(sprintf("%s/Factor3_bacteria_weights.pdf",io$outdir), width=4, height=5.5, useDingbats = F)
plot_weights(mofa, 
  factors = 3, 
  view = "Bacteria", 
  # manual = list("A"=opts$good.bacteria, "B"=opts$bad.bacteria),
  # color_manual = c("black","black"),
  text_size = 5
)
dev.off()

pdf(sprintf("%s/Factor3_fungi_weights.pdf",io$outdir), width=4, height=5.5, useDingbats = F)
plot_weights(mofa, 
  factors = 3, 
  view = "Fungi", 
  # manual = list("A"=opts$good.fungi, "B"=opts$bad.fungi),
  # color_manual = c("black","black"),
  text_size = 5
)
dev.off()

pdf(sprintf("%s/Factor3_viruses_weights.pdf",io$outdir), width=4, height=5.5, useDingbats = F)
plot_weights(mofa, 
  factors = 3, 
  view = "Viruses", 
  # manual = list("A"=opts$good.viruses, "B"=opts$bad.viruses),
  # color_manual = c("black","black"),
  text_size = 5
)
dev.off()

# plot_top_weights(mofa, factors=1, view="Viruses", sign = "positive", abs=F, nfeatures = 25)
# plot_top_weights(mofa, factors=1, view="Fungi", sign = "all", abs=F, nfeatures = 15)
# plot_top_weights(mofa, factors=1, view="Viruses", sign = "both", abs=T, scale=F, nfeatures = 50)

######################
## Plot top weights ##
######################

pdf(sprintf("%s/Factor3_fungi_top_weights.pdf",io$outdir), width=6, height=5, useDingbats = F)
plot_top_weights(mofa, 
  factors = 3, 
  view = "Fungi", 
  nfeatures = 10,
  sign = "both", 
  abs=F,
)
dev.off()


pdf(sprintf("%s/Factor3_bacteria_top_weights.pdf",io$outdir), width=6, height=7, useDingbats = F)
plot_top_weights(mofa, 
                 factors = 3, 
                 view = "Bacteria", 
                 nfeatures = 15,
                 sign = "both", 
                 abs=F,
)
dev.off()


pdf(sprintf("%s/Factor3_virus_top_weights.pdf",io$outdir), width=6, height=5, useDingbats = F)
plot_top_weights(mofa, 
                 factors = 3, 
                 view = "Viruses", 
                 nfeatures = 10,
                 sign = "both", 
                 abs=F,
)
dev.off()

###################
## Plot heatmaps ##
###################

## Bacteria ##
pdf(sprintf("%s/Factor3_bacteria_heatmap.pdf",io$outdir), width=7, height=5)
plot_data_heatmap(mofa, 
  factor = 3, 
  view = "Bacteria", 
  features = 20,
  # features = c(good.bacteria,bad.bacteria),
  # color = viridis(100),
  denoise = TRUE, 
  legend = FALSE,
  cluster_rows = T, cluster_cols = F,
  show_colnames = F, show_rownames = T,
  annotation_samples = "Category",  annotation_colors = list("Category"=opts$colors), annotation_legend = F,
  scale = "row"
)
dev.off()


## Fungi ##
pdf(sprintf("%s/Factor3_fungi_heatmap.pdf",io$outdir), width=7, height=5)
plot_data_heatmap(mofa, 
  factor = 3, 
  view = "Fungi", 
  features = 10,
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


## Viruses ##
pdf(sprintf("%s/Factor3_viruses_heatmap.pdf",io$outdir), width=7, height=5)
plot_data_heatmap(mofa, 
  factor = 3, 
  view = "Viruses", 
  features = 10,
  # features = c(opts$good.viruses,opts$bad.viruses),
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
nfeatures <- 4
nrow <- 2; ncol <- 2

for (sign in c("positive","negative")) {
  for (i in views_names(mofa)) {
    
    weights <- sort(get_weights(mofa, factor=3, views=i)[[1]][,1])
    if (sign=="positive") {
      features <- names( tail(weights,n=nfeatures) )
    } else {
      features <- names( head(weights,n=nfeatures) )
    }
    
    p_list <- list()
    for (j in features) {
      p_list[[j]] <- plot_factor(mofa, 
          factor = 3, 
          color_by = j, shape_by="Category",
          dot_size = 3.5, stroke = 0.4,
          legend = TRUE
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
    
    pdf(sprintf("%s/Factor3_%s_%s.pdf",io$outdir,i,sign), width=10, height=7, useDingbats = F)
    print(p)
    dev.off()
  }
}


