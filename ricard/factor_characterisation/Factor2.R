################
## Load model ##
################

source("/Users/ricard/MOFA_microbiome/ricard/load_model.R")

io$outdir <- "/Users/ricard/data/mofa_microbiome/pdf/Factor2"

opts$negative.fungi <- c(
    "Paraphaeosphaeria",
    "Agaricus",
    "Sclerotiniaceae",
    "Aureobasidium"
    # "Debaryomyces"
)

opts$positive.fungi <- c(
    # "Candida",
    "Aspergillus",
    # "Issatchenkia",
    "Penicillium",
    # "Piskurozyma",
    "Meyerozyma",
    "Rhodotorula"
)

########################
## Plot factor values ##
########################

p <- plot_factor(mofa, 
  factor = 2, 
  color_by = "Category", 
  dot_size = 5,
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

pdf(sprintf("%s/Factor2_category.pdf",io$outdir), width=6, height=5, useDingbats = F)
print(p)
dev.off()

p <- plot_factors(mofa, factors=c(1,2), color_by = "Category", dot_size = 3) +
  scale_fill_manual(values=opts$colors) +
  theme(
    legend.title = element_blank(),
  )

pdf(sprintf("%s/Factor1_vs_Factor2_category.pdf",io$outdir), width=7, height=5, useDingbats = F)
print(p)
dev.off()

##################
## Plot weights ##
##################

pdf(sprintf("%s/Factor2_fungi_weights.pdf",io$outdir), width=8, height=5.5, useDingbats = F)
plot_weights(mofa, 
  factors = 2, 
  view = "Fungi", 
  manual = list("A"=opts$positive.fungi, "B"=opts$negative.fungi),
  color_manual = c("black","black"),
  text_size = 7
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
  features = c(opts$positive.fungi,opts$negative.fungi),
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

nfeatures <- 4
nrow <- 2; ncol <- 2

# Fetch features with the largest weight  
weights <- sort(get_weights(mofa, factor=2, views="Fungi")[[1]][,1])

for (sign in c("positive","negative")) {
  
  if (sign=="positive") {
    features <- names( tail(weights, n=nfeatures) )
  } else if (sign=="negative") {
    features <- names( head(weights, n=nfeatures) )
  }
  
  p_list <- list()
  for (j in features) {
    p_list[[j]] <- plot_factor(mofa, 
        factor = 2, 
        color_by = j, shape_by="Category",
        dot_size = 3.5, stroke = 0.4,
        legend = FALSE,
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
}
    

#######################
## Plot data scatter ##
#######################


p <- plot_data_scatter(mofa,
  factor = 2, view = "Fungi",
  dot_size = 2,
  # features = nfeatures,
  # features = opts$positive.fungi,
  features = opts$negative.fungi,
  color_by = "Category",
  color_legend = FALSE
) 
p <- p + scale_color_manual(values=opts$colors) +
  theme(
    axis.title = element_text(size=rel(1.0)),
    axis.text = element_text(size=rel(0.8))
  )

pdf(sprintf("%s/Factor2_scatter_expr_%s.pdf",io$outdir,sign), width=5, height=7, useDingbats = F)
print(p)
dev.off()
