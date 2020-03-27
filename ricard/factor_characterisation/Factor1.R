################
## Load model ##
################

source("/Users/ricard/MOFA_microbiome/ricard/load_model.R")

io$outdir <- "/Users/ricard/data/mofa_microbiome/pdf/Factor1"

#####################
## Define settings ##
#####################

opts$good.bacteria <- c(
  "Agathobacter",
  "Roseburia",
  "Faecalibacterium",
  "Ruminococcus",
  "Coprococcus",
  # "Butyricicoccus",
  "Blautia",
  # "Anaerostipes",
  # "Bifidobacterium",
  "Lachnospiraceae"
  # "Ruminococcaceae"
)

opts$bad.bacteria <- c(
  "Enterococcus", 
  "Klebsiella",
  "Staphylococcus",
  "Enterobacter",
  "Erysipelotrichaceae",
  "Escherichia/Shigella"
)

opts$bad.fungi <- c(
  "Saccharomyces",
  "Aspergillus",
  "Penicillium",
  "Debaryomyces",
  "Candida"
)

opts$good.fungi <- c(
  "Dipodascus",
  "Vishniacozyma",
  "Filobasidium",
  "Paraphaeosphaeria"
)

opts$good.viruses <- c(
  "Megavirales",
  "Rhodococcus phage",
  "Sphingomonas phage",
  "Picobirnavirus"
)

opts$bad.viruses <- c(
  "Enterococcus phage",
  "Enterobacteriaceae phage",
  "Escherichia/Shigella phage",
  "Klebsiella phage",
  "Staphylococcus phage"
)


########################
## Plot factor values ##
########################

p <- plot_factor(mofa, 
  factor = 1, 
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

pdf(sprintf("%s/Factor1_category.pdf",io$outdir), width=2.5, height=5, useDingbats = F)
print(p)
dev.off()

##################
## Plot weights ##
##################

## Bacteria ##

pdf(sprintf("%s/Factor1_bacteria_weights.pdf",io$outdir), width=4, height=5.5, useDingbats = F)
plot_weights(mofa, 
  factors = 1, 
  view = "Bacteria", 
  manual = list("A"=opts$good.bacteria, "B"=opts$bad.bacteria),
  color_manual = c("black","black"),
  text_size = 5.5
)
dev.off()

pdf(sprintf("%s/Factor1_fungi_weights.pdf",io$outdir), width=4, height=5.5, useDingbats = F)
plot_weights(mofa, 
  factors = 1, 
  view = "Fungi", 
  dot_size = 2,
  manual = list("A"=opts$good.fungi, "B"=opts$bad.fungi),
  color_manual = c("black","black"),
  text_size = 5.5
)
dev.off()

pdf(sprintf("%s/Factor1_viruses_weights.pdf",io$outdir), width=4, height=5.5, useDingbats = F)
plot_weights(mofa, 
  factors = 1, 
  dot_size = 1.5,
  view = "Viruses", 
  manual = list("A"=opts$good.viruses, "B"=opts$bad.viruses),
  color_manual = c("black","black"),
  text_size = 5.5
)
dev.off()


######################
## Plot top weights ##
######################

plot_top_weights(mofa, factors=1, view="Viruses", sign = "positive", abs=F, nfeatures = 25)
plot_top_weights(mofa, factors=1, view="Fungi", sign = "all", abs=F, nfeatures = 15)
plot_top_weights(mofa, factors=1, view="Viruses", sign = "both", abs=T, scale=F, nfeatures = 50)

###################
## Plot heatmaps ##
###################

## Bacteria ##

pdf(sprintf("%s/Factor1_bacteria_heatmap.pdf",io$outdir), width=5, height=5)
plot_data_heatmap(mofa, 
  factor = 1, 
  view = "Bacteria", 
  features = c(opts$good.bacteria,opts$bad.bacteria),
  denoise = TRUE, 
  legend = TRUE,
  cluster_rows = T, cluster_cols = F,
  show_colnames = F, show_rownames = T,
  annotation_samples = "Category",  annotation_colors = list("Category"=opts$colors), annotation_legend = F,
  scale = "row"
)
dev.off()


## Fungi ##

pdf(sprintf("%s/Factor1_fungi_heatmap.pdf",io$outdir), width=5, height=5)
plot_data_heatmap(mofa, 
  factor = 1, 
  view = "Fungi", 
  # features = 20,
  features = c(opts$good.fungi,opts$bad.fungi),
  denoise = TRUE, 
  legend = TRUE,
  cluster_rows = T, cluster_cols = F,
  show_colnames = F, show_rownames = T,
  annotation_samples = "Category",  annotation_colors = list("Category"=opts$colors), annotation_legend = F,
  scale = "row"
)
dev.off()


## Viruses ##

pdf(sprintf("%s/Factor1_viruses_heatmap.pdf",io$outdir), width=5, height=5)
plot_data_heatmap(mofa,
  factor = 1,
  view = "Viruses",
  # features = c(opts$good.viruses,opts$bad.viruses),
  features = opts$bad.viruses,
  denoise = TRUE,
  legend = TRUE,
  min.value = 0, max.value = 6,
  cluster_rows = F, cluster_cols = F,
  show_colnames = F, show_rownames = T,
  annotation_samples = "Category",  annotation_colors = list("Category"=opts$colors), annotation_legend = F
)
dev.off()


# p <- plot_data_scatter(
#   mofa, 
#   factor = 1, 
#   view = "Viruses", 
#   # features = opts$bad.viruses,
#   features = "Enterococcus phage",
#   color_by = "Category",
#   legend = FALSE,
#   stroke = 0.5
# ) + scale_fill_manual(values=opts$colors) 
# 
# pdf(sprintf("%s/Factor1_virus_scatter.pdf",io$outdir), width=5, height=5, useDingbats = F)
# print(p)
# dev.off()


#########################################
## Plot factor values coloured by OTUs ##
#########################################

# # plot factor values coloured by features with top weights
# nfeatures <- 4
# nrow <- 2; ncol <- 2
# 
# for (sign in c("positive","negative")) {
#   for (i in views(mofa)) {
#     
#     weights <- sort(get_weights(mofa, factor=1, views=i)[[1]][,1])
#     if (sign=="positive") {
#       features <- names( tail(weights,n=nfeatures) )
#     } else {
#       features <- names( head(weights,n=nfeatures) )
#     }
#     
#     p_list <- list()
#     for (j in features) {
#       p_list[[j]] <- plot_factors(mofa, c(1,3), color_by = j, dot_size = 3.0, legend=FALSE) +
#         ggtitle(j) +
#         theme(
#           plot.title = element_text(hjust = 0.5, size=rel(1.2)),
#           axis.title = element_text(size=rel(0.8)),
#           axis.text = element_text(size=rel(0.8))
#         )
#     }
#     p <- cowplot::plot_grid(plotlist=p_list, nrow=nrow, ncol=ncol)
#     
#     pdf(sprintf("%s/Factor1_vs_Factor3_%s_%s.pdf",io$outdir,i,sign), width=8, height=7, useDingbats = F)
#     print(p)
#     dev.off()
#       
#   }
# }
