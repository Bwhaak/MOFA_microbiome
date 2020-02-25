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