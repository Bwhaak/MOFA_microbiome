################
## Load model ##
################

source("/Users/ricard/MOFA_microbiome/ricard/load_settings.R")
source("/Users/ricard/MOFA_microbiome/ricard/load_model.R")

###################################################################
## Scatterplots of Bacteria vs corresponding Virus phage weights ##
###################################################################

Viruses <- features_names(mofa)[["Viruses"]]
Viruses <- Viruses[grep("phage",Viruses)]
Viruses <- stringr::str_replace_all(Viruses, " phage","")
Bacteria <- features_names(mofa)[["Bacteria"]]
Viruses_phages <- Viruses[Viruses %in% Bacteria]

weights.dt <- get_weights(mofa, views=c("Bacteria","Viruses"), factors=c(1,3), as.data.frame = T) %>%
  as.data.table %>%
  .[,feature:=stringr::str_replace_all(feature, " phage","")] %>%
  .[feature%in%Viruses_phages] %>%
  .[,value:=value/max(value), by="view"] %>%
  dcast(feature+factor~view, value.var="value")

p <- ggscatter(weights.dt, x="Bacteria", y="Viruses",
  add="reg.line", add.params = list(color="blue", fill="lightgray"), conf.int=TRUE) +
  facet_wrap(~factor) +
  # scale_x_continuous(breaks=c(0,1)) +
  # coord_cartesian(xlim=c(-1,1), ylim=c(-1,1)) +
  stat_cor(method = "pearson") +
  ggrepel::geom_text_repel(
    aes_string(label = "feature"),
    size=4, segment.alpha=0.25, segment.color="black", segment.size=0.3, 
    box.padding = unit(0.5,"lines")) +
  labs(x="Weight in Factor 1 (Viruses)", y="Weight in Factor 1 (Bacteria)")
  

pdf(sprintf("%s/Bacteria_vs_virus_weights.pdf",io$outdir), width=10, height=6, useDingbats = F)
print(p)
dev.off()


###############################################################
## Scatterplots Bacteria vs corresponding Virus phage levels ##
###############################################################

data.dt <- get_data(mofa, views=c("Bacteria","Viruses"), as.data.frame = T) %>%
  as.data.table %>%
  .[,feature:=stringr::str_replace_all(feature, " phage","")] %>%
  .[feature%in%Viruses_phages] %>%
  dcast(sample+feature~view, value.var="value") %>%
  .[complete.cases(.)] %>%
  merge(metadata[,c("sample","Category")], by="sample")

p <- ggscatter(data.dt, x="Bacteria", y="Viruses", color="Category",
          add="reg.line", add.params = list(color="blue", fill="lightgray"), conf.int=TRUE) +
  ggpubr::stat_cor(method = "pearson", label.sep="\n", output.type = "latex", size = 3, color = "black") +
  scale_color_manual(values=opts$colors) +
  facet_wrap(~feature, scales="free") +
  labs(x="Scaled ASV levels (Bacteria)", y="Scaled contig levels (Viruses)") +
  theme(
    legend.position = "none",
    axis.text = element_text(size=rel(0.7))
  )

pdf(sprintf("%s/Bacteria_vs_Virus_data.pdf",io$outdir), width=10, height=9, useDingbats = F)
print(p)
dev.off()

