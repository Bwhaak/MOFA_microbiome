library(MOFA2)
library(data.table)
library(purrr)
library(ggpubr)

#####################
## Define settings ##
#####################

io <- list()
# io$mofa <- "/Users/ricard/data/gastrulation/mofa2/hdf5/test_1.hdf5"
io$outdir <- "/Users/ricard/data/mofa_microbiome/pdf"

################
## Load model ##
################

# mofa <- load_model(file = io$mofa)

plot_factor_cor(mofa)

# subset factors
# mofa <- subset_factors(mofa, factors=1:10)

# rename views
tmp <- c(
  "bacteria" = "Bacteria",
  "fungi" = "Fungi",
  "viruses" = "Viruses"
)
views(mofa) = stringr::str_replace_all(views(mofa), tmp[views(mofa)])

#######################################################
## Plot variance explained per view vs factor number ##
#######################################################

r2 <- mofa@cache$variance_explained$r2_per_factor[[1]]

r2.dt <- r2 %>%
  as.data.table %>% .[,factor:=as.factor(1:mofa@dimensions$K)] %>%
  melt(id.vars=c("factor"),
       variable.name="view", value.name = "r2") %>%
  .[,r2:=r2*100] %>%
  .[,cum_r2:=cumsum(r2), by="view"]

threshold.var <- 0.05
max.factor <- max(which(apply(r2,1,sum) >= threshold.var))

p <- ggline(r2.dt, x="factor", y="cum_r2", color="view") +
  scale_color_brewer(palette = "Dark2") +
  labs(x="Factor number", y="Cumulative variance explained (%)") +
  geom_vline(xintercept = max.factor, linetype="dashed") +
  # theme(legend.title = element_blank())
  theme(legend.title = element_blank(), legend.position = "top")

pdf(paste0(io$outdir,"/r2_vs_factor.pdf"), width=6, height=4, useDingbats = F)
print(p)
dev.off()
