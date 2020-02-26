source("/Users/ricard/MOFA_microbiome/ricard/load_settings.R")
source("/Users/ricard/MOFA_microbiome/ricard/load_model.R")

opts$positive.bacteria <- c(
  "Escherichia/Shigella",
  "Olsenella",
  # "Terrisporobacter",
  # "Intestinibacter",
  "Desulfovibrio",
  "Slackia",
  "Christensenellaceae"
)


opts$negative.bacteria <- c(
  "Pediococcus",
  "Lactococcus",
  "Megasphaera",
  "Lachnospiraceae_UCG-003",
  "Megamonas"
  # "Roseburia",
  # "Agathobacter"
)

opts$fungi <- c("Aspergillus")

########################
## Plot factor values ##
########################

# Scatterplot
p <- plot_factors(mofa, c(1,4), color_by = "Category", dot_size = 3) +
  scale_fill_manual(values=opts$colors) +
  theme(
    legend.title = element_blank(),
  )

pdf(sprintf("%s/Factor1_vs_Factor4_category.pdf",io$outdir), width=7, height=5, useDingbats = F)
print(p)
dev.off()

# Beeswarmplot
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


##################################
## Association with antibiotics ##
##################################

p1 <- plot_factor(mofa, factors = 4, color_by = "Cephalosporins", dot_size = 4, add_violin = TRUE, dodge=TRUE, legend = TRUE) +
  scale_fill_manual(values=c("TRUE"="blue","FALSE"="red")) +
  theme(
    legend.position = "top",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

p2 <- plot_factor(mofa, factors = 4, color_by = "Quinolones", dot_size = 4, add_violin = TRUE, dodge=TRUE, legend = TRUE) +
  scale_fill_manual(values=c("TRUE"="purple","FALSE"="orange")) +
  theme(
    legend.position = "top",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


pdf(sprintf("%s/Factor4_factor_vs_antibiotics.pdf",io$outdir), width=8, height=5, useDingbats = F)
cowplot::plot_grid(plotlist=list(p1,p2))
dev.off()


##################
## Plot weights ##
##################

## Bacteria ##

p1 <- plot_weights(mofa, 
  factors = 4, 
  view = "Bacteria", 
  nfeatures = 10,
  manual = list("A"=opts$positive.bacteria, "B"=opts$negative.bacteria),
  color_manual = c("black","black"),
  text_size = 4
)
pdf(sprintf("%s/Factor4_bacteria_weights.pdf",io$outdir), width=4, height=5.5, useDingbats = F)
print(p1)
dev.off()


p2 <- plot_weights(mofa, 
  factors = 4, 
  view = "Fungi", 
  manual = list(opts$fungi),
  text_size = 5,
  dot_size = 2.5
)
pdf(sprintf("%s/Factor4_fungi_weights.pdf",io$outdir), width=4, height=5.5, useDingbats = F)
print(p2)
dev.off()

pdf(sprintf("%s/Factor4_weights.pdf",io$outdir), width=8, height=5, useDingbats = F)
cowplot::plot_grid(plotlist=list(p1,p2))
dev.off()


###################
## Plot heatmaps ##
###################

## Bacteria ##

pdf(sprintf("%s/Factor4_bacteria_heatmap.pdf",io$outdir), width=8, height=5)
plot_data_heatmap(mofa, 
  factor = 4, 
  view = "Bacteria", 
  # features = 35,
  features = c(opts$positive.bacteria,opts$negative.bacteria),
  denoise = TRUE, 
  legend = FALSE,
  cluster_rows = T, cluster_cols = F,
  show_colnames = F, show_rownames = T,
  annotation_samples = c("Cephalosporins","Quinolones","Category"),  
  annotation_colors = list("Category"=opts$colors, "Cephalosporins"=c("TRUE"="blue","FALSE"="red"), "Quinolones"=c("TRUE"="purple","FALSE"="orange")), 
  annotation_legend = F,
  scale = "row"
)
dev.off()


## Fungi ##

# pdf(sprintf("%s/Factor4_fungi_heatmap.pdf",io$outdir), width=7, height=5)
# plot_data_heatmap(mofa, 
#   factor = 4, 
#   view = "Fungi", 
#   features = opts$fungi,
#   denoise = TRUE, 
#   legend = FALSE,
#   cluster_rows = T, cluster_cols = F,
#   show_colnames = F, show_rownames = T,
#   annotation_samples = c("Cephalosporins","Category"),  
#   annotation_colors = list("Category"=opts$colors, "Cephalosporins"=c("TRUE"="blue","FALSE"="red")), 
#   annotation_legend = F,
#   scale = "row"
# )
# dev.off()

#######################
## Plot data scatter ##
#######################

p <- plot_data_scatter(mofa,
  factor = 4, 
  view = "Fungi",
  dot_size = 3,
  features = opts$fungi,
  color_by = "Category",
  color_legend = FALSE
) 
p <- p + scale_color_manual(values=opts$colors) +
  theme(
    axis.title = element_text(size=rel(1.0)),
    axis.text = element_text(size=rel(0.8))
  )

pdf(sprintf("%s/Factor4_scatter_Asperigullus.pdf",io$outdir), width=7, height=6, useDingbats = F)
print(p)
dev.off()
