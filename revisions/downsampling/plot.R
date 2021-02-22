library(pheatmap)

#####################
## Define settings ##
#####################

source("/Users/ricard/MOFA_microbiome/load_settings.R")

# I/O
io$model.dir <- paste0(io$basedir,"/results/downsampling")
io$outdir <- paste0(io$basedir,"/results/downsampling/pdf")

# Options
opts$downsampling.fraction <- seq(from=0.50, to=1, by=0.05)
opts$trials <- 10


##########################################################################################
## Load MOFA models and calculate correlations between downsampled and original factors ##
##########################################################################################

# Load original model
# source("/Users/ricard/MOFA_microbiome/3_downstream_analysis/load_model.R")
original.mofa <- load_model(io$mofa.hdf5)
original.factors <- get_factors(original.mofa)[[1]]

# Load downsampled models
# mofa <- list()
cor_output <- list()
for (i in opts$downsampling.fraction) {
  # mofa[[as.character(i)]] <- list()
  for (j in 1:opts$trials) {
    model.file <- sprintf("%s/%s_%s.hdf5", io$model.dir,i,j)
    # mofa[[as.character(i)]][[as.character(j)]] <- load_model(model.file)
    mofa <- load_model(model.file)
    
    # Get factors
    factors <- get_factors(mofa)[[1]]
    # factors <- get_factors(mofa[[as.character(i)]][[as.character(j)]])[[1]]
    
    # Correlate factors
    r <- cor(factors,original.factors[rownames(factors),]) %>% abs
    r <- r[,apply(r,1,which.max)]
    rownames(r) <- paste("Factor",1:nrow(r), sep=" "); colnames(r) <- paste("Factor",1:ncol(r), sep=" ")
    cor_output[[sprintf("%s_%s",i,j)]] <- data.table(downsampling_fraction=i, trial=j, factor=rownames(r), cor=diag(r))
    
    pdf(sprintf("%s/downsampling%s_trial%s.pdf", io$outdir,i,j))
    pheatmap(r,
      color = colorRampPalette(c("white",RColorBrewer::brewer.pal(n = 7, name = "YlOrRd")))(100),
      cluster_rows = F, cluster_cols = F
    )
    dev.off()
    # 
    
  }
}


##########
## Plot ##
##########

to.plot <- rbindlist(cor_output) %>%
  .[,factor:=factor(factor,levels=rownames(r))] %>%
  .[,downsampling_fraction:=as.]

unique(to.plot$downsampling_fraction)


p <- ggline(to.plot, x="downsampling_fraction", y="cor", add = "mean_se", facet="factor", ncol=5) +
  coord_cartesian(ylim=c(0,1.01)) +
  scale_x_discrete(breaks=c("0.5","0.75","1")) +
  labs(x="Downsampling fraction", y="Pearson correlation with original factors") +
  theme(
    axis.text = element_text(size=rel(0.75)),
    axis.ticks = element_line(size=rel(0.75))
  )

pdf(sprintf("%s/lineplot_downsampling_per_factor.pdf", io$outdir), width = 8, height=4)
print(p)
dev.off()