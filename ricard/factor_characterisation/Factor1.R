source("/Users/ricard/MOFA_microbiome/ricard/load_settings.R")
source("/Users/ricard/MOFA_microbiome/ricard/load_model.R")

########################
## Plot factor values ##
########################

p <- plot_factors(mofa, c(1,3), color_by = "Category", dot_size = 3) +
  # scale_fill_brewer(palette = "Set2") +
  scale_fill_manual(values=opts$colors) +
  theme(
    legend.title = element_blank(),
  )

pdf(sprintf("%s/Factor1_vs_Factor3_category.pdf",io$outdir), width=7, height=5, useDingbats = F)
print(p)
dev.off()

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
    legend.position = "right",
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
  text_size = 5
)
dev.off()

pdf(sprintf("%s/Factor1_fungi_weights.pdf",io$outdir), width=4, height=5.5, useDingbats = F)
plot_weights(mofa, 
  factors = 1, 
  view = "Fungi", 
  manual = list("A"=opts$good.fungi, "B"=opts$bad.fungi),
  color_manual = c("black","black"),
  text_size = 5
)
dev.off()

pdf(sprintf("%s/Factor1_viruses_weights.pdf",io$outdir), width=4, height=5.5, useDingbats = F)
plot_weights(mofa, 
  factors = 1, 
  view = "Viruses", 
  manual = list("A"=opts$good.viruses, "B"=opts$bad.viruses),
  color_manual = c("black","black"),
  text_size = 5
)
dev.off()

plot_top_weights(mofa, factors=1, view="Viruses", sign = "positive", abs=F, nfeatures = 25)
plot_top_weights(mofa, factors=1, view="Fungi", sign = "all", abs=F, nfeatures = 15)

plot_top_weights(mofa, factors=1, view="Viruses", sign = "both", abs=T, scale=F, nfeatures = 50)

## test
w <- get_weights(mofa, views = "Bacteria", scale = T)[[1]]
w[grep("crAss phage",rownames(w)),]
w[grep("Ruminococcaceae",rownames(w)),]

corrplot(cor(t(w[grep("Buty",rownames(w)),])))
## test

###################
## Plot heatmaps ##
###################

## Bacteria ##

pdf(sprintf("%s/Factor1_bacteria_heatmap.pdf",io$outdir), width=7, height=5)
plot_data_heatmap(mofa, 
  factor = 1, 
  view = "Bacteria", 
  features = c(good.bacteria,bad.bacteria),
  # color = viridis(100),
  denoise = TRUE, 
  legend = TRUE,
  cluster_rows = T, cluster_cols = F,
  show_colnames = F, show_rownames = T,
  annotation_samples = "Category",  annotation_colors = list("Category"=opts$colors), annotation_legend = F,
  scale = "row"
)
dev.off()


## Fungi ##


pdf(sprintf("%s/Factor1_fungi_heatmap.pdf",io$outdir), width=7, height=5)
plot_data_heatmap(mofa, 
  factor = 1, 
  view = "Fungi", 
  features = c(opts$good.fungi,opts$bad.fungi),
  # color = viridis(100),
  denoise = TRUE, 
  legend = TRUE,
  cluster_rows = T, cluster_cols = F,
  show_colnames = F, show_rownames = T,
  annotation_samples = "Category",  annotation_colors = list("Category"=opts$colors), annotation_legend = F,
  scale = "row"
)
dev.off()


## Viruses ##

pdf(sprintf("%s/Factor1_virsues_heatmap.pdf",io$outdir), width=7, height=5)
plot_data_heatmap(mofa, 
  factor = 1, 
  view = "Viruses", 
  features = 30,
  # features = c(opts$good.viruses,opts$bad.viruses),
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
  
  pdf(sprintf("%s/Factor1_vs_Factor3_%s_%s.pdf",io$outdir,i,sign), width=8, height=7, useDingbats = F)
  print(p)
  dev.off()
    
}


#########################################
## Correlate Bacteria vs phage weights ##
#########################################

Viruses <- features(mofa)[["Viruses"]]
Viruses <- Viruses[grep("phage",Viruses)]
Viruses <- stringr::str_replace_all(Viruses, " phage","")
Bacteria <- features(mofa)[["Bacteria"]]
Viruses_phages <- Viruses[Viruses %in% Bacteria]

weights.dt <- get_weights(mofa, views=c("Bacteria","Viruses"), factors="all", as.data.frame = T) %>%
  as.data.table %>%
  .[,feature:=stringr::str_replace_all(feature, " phage","")] %>%
  .[feature%in%Viruses_phages] %>%
  .[,value:=value/max(value), by="view"] %>%
  .[,value:=abs(value), by="view"] %>%
  dcast(feature+factor~view, value.var="value")

p <- ggscatter(weights.dt[factor=="Factor1"], x="Bacteria", y="Viruses",
  add="reg.line", add.params = list(color="blue", fill="lightgray"), conf.int=TRUE) +
  coord_cartesian(xlim=c(0,1), ylim=c(0,1)) +
  stat_cor(method = "pearson") +
  labs(x="Weight in Factor 1 (Viruses)", y="Weight in Factor 1 (Bacteria)")
  

pdf(sprintf("%s/Bacteria_vs_virus_Factor1.pdf",io$outdir), width=6, height=5, useDingbats = F)
print(p)
dev.off()



