library(ggpubr)

################
## Load model ##
################

source("/Users/ricard/MOFA_microbiome/ricard/load_model.R")
io$outdir <- paste0(io$basedir,"/results/r2")

#######################################################
## Plot cumulative variance explained per view vs factor number ##
#######################################################

r2.dt <- mofa@cache$variance_explained$r2_per_factor[[1]] %>%
  as.data.table %>% .[,factor:=as.factor(1:mofa@dimensions$K)] %>%
  melt(id.vars="factor", variable.name="view", value.name = "r2") %>%
  .[,cum_r2:=cumsum(r2), by="view"]

threshold.var <- 5
max.factor <- max(which(apply(r2,1,sum) >= threshold.var))

p <- ggline(r2.dt, x="factor", y="cum_r2", color="view") +
  scale_color_manual(values=opts$colors.views) +
  labs(x="Factor number", y="Cumulative variance explained (%)") +
  geom_vline(xintercept = max.factor, linetype="dashed") +
  theme(
    legend.title = element_blank(), 
    legend.position = "top",
    axis.text = element_text(size=rel(0.8))
  )

pdf(paste0(io$outdir,"/r2_vs_factor.pdf"), width=8, height=5, useDingbats = F)
print(p)
dev.off()


####################################################
## Plot total variance explained vs factor number ##
####################################################

r2.dt <- mofa@cache$variance_explained$r2_per_factor[[1]] %>%
  as.data.table %>% .[,factor:=as.factor(1:mofa@dimensions$K)] %>%
  melt(id.vars="factor", variable.name="view", value.name = "r2") %>%
  .[,r2:=sum(r2),by="factor"]

p <- ggline(r2.dt, x="factor", y="r2") +
  labs(x="Factor number", y="Total variance explained (%)") +
  geom_vline(xintercept = max.factor, linetype="dashed") +
  theme(
    legend.title = element_blank(), 
    legend.position = "top",
    axis.text = element_text(size=rel(0.8))
  )

pdf(paste0(io$outdir,"/r2_vs_factor.pdf"), width=8, height=5, useDingbats = F)
print(p)
dev.off()
