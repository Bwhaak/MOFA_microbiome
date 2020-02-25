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

p <- ggscatter(weights.dt[factor=="Factor3"], x="Bacteria", y="Viruses",
  add="reg.line", add.params = list(color="blue", fill="lightgray"), conf.int=TRUE) +
  coord_cartesian(xlim=c(0,1), ylim=c(0,1)) +
  stat_cor(method = "pearson") +
  labs(x="Weight in Factor 1 (Viruses)", y="Weight in Factor 1 (Bacteria)")
  

pdf(sprintf("%s/Bacteria_vs_virus_Factor3.pdf",io$outdir), width=6, height=5, useDingbats = F)
print(p)
dev.off()



