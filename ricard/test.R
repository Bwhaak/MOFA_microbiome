
# subset factors
threshold.var <- 0.05
factors <- which(apply(r2,1,sum) >= threshold.var)
mofa <- subset_factors(mofa, factors)


# plot variance explained
p <- plot_variance_explained(mofa, max_r2=0.2)


pdf(paste0(io$outdir,"/var_explained.pdf"), width=6, height=5, useDingbats = F)
print(p)
dev.off()


colnames(foo)

p <- plot_factors(mofa, c(1,3), color_by = "Category", dot_size = 2.5)

pdf(sprintf("%s/Factor1_vs_Factor3_category.pdf",io$outdir,j), width=6, height=4, useDingbats = F)
print(p)
dev.off()


plot_weights(mofa, factors=1, view="viruses")

plot_top_weights(mofa, factors=1, view="bacteria", sign = "positive", abs=F)

 mofa <- run_umap(mofa)
mofa <- run_tsne(mofa, perplexity=5)

p <- plot_dimred(mofa, method="UMAP", color_by = "Category", dot_size = 2.5)

pdf(paste0(io$outdir,"/umap.pdf"), width=7, height=5, useDingbats = F)
print(p)
dev.off()

p <- plot_dimred(mofa, method="TSNE", color_by = "Category", dot_size = 2.5)

pdf(paste0(io$outdir,"/tsne.pdf"), width=7, height=5, useDingbats = F)
print(p)
dev.off()


p <- plot_dimred(mofa, method="TSNE", color_by = "Category", dot_size = 2.5)

pdf(paste0(io$outdir,"/tsne.pdf"), width=7, height=5, useDingbats = F)
print(p)
dev.off()

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
## Correlate bacteria vs phage weights ##
#########################################

viruses <- features(mofa)[["viruses"]]
viruses <- viruses[grep("phage",viruses)]
viruses <- stringr::str_replace_all(viruses, " phage","")
bacteria <- features(mofa)[["bacteria"]]
viruses_phages <- viruses[viruses %in% bacteria]

weights.dt <- get_weights(mofa, views=c("bacteria","viruses"), factors="all", as.data.frame = T) %>%
  as.data.table %>%
  .[,feature:=stringr::str_replace_all(feature, " phage","")] %>%
  .[feature%in%viruses_phages] %>%
  .[,value:=value/max(value), by="view"] %>%
  .[,value:=abs(value), by="view"] %>%
  dcast(feature+factor~view, value.var="value")

p <- ggscatter(weights.dt[factor=="Factor1"], x="bacteria", y="viruses",
  add="reg.line", add.params = list(color="blue", fill="lightgray"), conf.int=TRUE) +
  coord_cartesian(xlim=c(0,1), ylim=c(0,1)) +
  stat_cor(method = "pearson") +
  labs(x="Weight in Factor 1 (viruses)", y="Weight in Factor 1 (bacteria)")
  

pdf(sprintf("%s/bacteria_vs_virus_Factor1.pdf",io$outdir), width=6, height=5, useDingbats = F)
print(p)
dev.off()

##################################
## Characterisation of Factor 2 ##
##################################

p <- plot_factor(mofa, 
  factors=2, 
  color_by = "Category", 
  group_by = "Category", 
  dot_size = 3, 
  add_violin = TRUE, 
  legend = FALSE
)
p  

pdf(sprintf("%s/Factor2_values.pdf",io$outdir), width=6, height=4, useDingbats = F)
print(p)
dev.off()

p <- plot_weights(mofa, factors=2, view="fungi", nfeatures = 6)

pdf(sprintf("%s/Factor2_weights.pdf",io$outdir), width=5, height=4, useDingbats = F)
print(p)
dev.off()

sign <- "positive"
# sign <- "negative"
p <- plot_data_scatter(mofa, factor=2, view="fungi", features = 4, sign = sign)

pdf(sprintf("%s/Factor2_data_scatter_%s.pdf",io$outdir,sign), width=5, height=4, useDingbats = F)
print(p)
dev.off()



##################################
## Characterisation of Factor 4 ##
##################################

plot_factors(mofa, c(1,4), color_by = "Category", dot_size = 2.5)

p <- plot_factor(mofa, 
                 factors = 4, 
                 color_by = "Category", 
                 group_by = "Category", 
                 dot_size = 3, 
                 add_violin = TRUE, 
                 legend = FALSE
)

plot_factor(mofa, factors = 4, color_by = "CAG-56", dot_size = 3, add_violin = TRUE, dodge=TRUE, legend = TRUE)
plot_factor(mofa, factors = 4, color_by = "Quinolones", dot_size = 3, add_violin = TRUE, dodge=TRUE, legend = TRUE)

pdf(sprintf("%s/Factor4_values.pdf",io$outdir), width=6, height=4, useDingbats = F)
print(p)
dev.off()

p <- plot_weights(mofa, factors=4, view="bacteria", nfeatures = 6)
p <- plot_top_weights(mofa, factors=4, view="bacteria", nfeatures = 10)

pdf(sprintf("%s/Factor4_weights.pdf",io$outdir), width=5, height=4, useDingbats = F)
print(p)
dev.off()

sign <- "positive"
# sign <- "negative"
p <- plot_data_scatter(mofa, factor=4, view="bacteria", features = 6, sign = sign)

pdf(sprintf("%s/Factor4_data_scatter_%s.pdf",io$outdir,sign), width=7, height=6, useDingbats = F)
print(p)
dev.off()

##################################
## Characterisation of Factor 5 ##
##################################

plot_factors(mofa, c(1,4), color_by = "Category", dot_size = 2.5)

p <- plot_factor(mofa, 
                 factors = 5, 
                 color_by = "Category", 
                 group_by = "Category", 
                 dot_size = 3, 
                 add_violin = TRUE, 
                 legend = FALSE
)

pdf(sprintf("%s/Factor5_values.pdf",io$outdir), width=6, height=4, useDingbats = F)
print(p)
dev.off()

p <- plot_weights(mofa, factors=5, view="bacteria", nfeatures = 6)
p <- plot_top_weights(mofa, factors=5, view="bacteria", nfeatures = 10)

pdf(sprintf("%s/Factor5_weights.pdf",io$outdir), width=5, height=4, useDingbats = F)
print(p)
dev.off()

sign <- "positive"
# sign <- "negative"
p <- plot_data_scatter(mofa, factor=5, view="bacteria", features = 6, sign = sign)

pdf(sprintf("%s/Factor5_data_scatter_%s.pdf",io$outdir,sign), width=7, height=6, useDingbats = F)
print(p)
dev.off()