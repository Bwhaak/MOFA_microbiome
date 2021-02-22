p <- plot_variance_explained(mofa, max_r2=14)

pdf(paste0(io$outdir,"/var_explained.pdf"), width=6, height=5, useDingbats = F)
print(p)
dev.off()



# plot_variance_explained_per_feature(mofa, view="Bacteria", features = "all", split_by_factor = T)

                                    