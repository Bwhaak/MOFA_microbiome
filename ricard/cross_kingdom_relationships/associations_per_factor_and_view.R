################
## Load model ##
################

source("/Users/ricard/MOFA_microbiome/ricard/load_settings.R")
source("/Users/ricard/MOFA_microbiome/ricard/load_model.R")

opts$fdr_threshold <- 0.10

################################################################
## Number of significant associations between view and factor ##
################################################################

data.dt <- get_data(mofa, as.data.frame = T) %>% as.data.table %>% .[,group:=NULL]
factor.dt <- get_factors(mofa, as.data.frame = T) %>% as.data.table %>% .[,group:=NULL]

dt <- merge(data.dt, factor.dt, by=c("sample"), allow.cartesian = T)

foo <- dt[, .(p = cor.test(value.x, value.y, method = "pearson")[["p.value"]]), by=c("feature","view","factor")] %>%
  .[,padj_fdr := p.adjust(p, method="fdr"), by="view"] %>%
  .[,sig:=padj_fdr<opts$fdr_threshold] %>%
  .[,round(mean(sig),2), by=c("view","factor")]

p <- ggbarplot(foo, x="factor", y="V1", fill="view", color="black",
          palette = "Set1", label = TRUE, lab.col = "white", lab.pos = "in") +
  labs(x="", y="Fraction of significant associations") +
  theme(
   legend.title = element_blank(),
    # axis.text.x = element_text(size=rel(0.8), color="black", angle=50, vjust=1, hjust=1)
    axis.text.x = element_text(size=rel(1.0), color="black")
  )

pdf(sprintf("%s/Associations_ViewVsFactor.pdf",io$outdir), width=7, height=5, useDingbats = F)
print(p)
dev.off()

