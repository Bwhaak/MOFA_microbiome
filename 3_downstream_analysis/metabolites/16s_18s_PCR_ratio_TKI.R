rm(list= ls())
library(ggplot2)
library(dplyr)
library(readxl)
library(data.table)
library(vegan)
library(reshape2)
library(scales)
library(gridExtra)
library(ggpubr)
library(yingtools2)

opts <- list()
RColorBrewer::brewer.pal(n = 4, name = 'Set2')
opts$colors <- c(
  "Sepsis" = "#E78AC3",
  "Non septic ICU" = "#FC8D62",
  "Healthy, no antibiotics" = "#66C2A5", 
  "Healthy, antibiotics" = "#8DA0CB")

give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}


pcr <- read_xlsx("D:/Data/Documents/PhD/Transkingdom interactions ICU/Metadata/Ricard_16S_PCR.xlsx", sheet = "Blad1")%>%
  left_join(metadata)%>%
  select(seqID, Category, corrected_16s, corrected_18s, def_fungal_bacterial_ratio, Butyrate_mg_feces, Acetate_mg_feces, Propionate_mg_feces)%>%
  melt(id.vars = c("seqID", "Category"))%>%
  mutate(value = as.numeric(value))


pcr$Category <- factor(pcr$Category, levels = c("Healthy, no antibiotics", "Healthy, antibiotics", "Non septic ICU", "Sepsis"))
#pcr$abx <- factor(pcr$abx, levels = c( "0", "<3 classes", "3-5 classes", ">5 classes"))
lev <- levels(pcr$Category) # get the variables
L.pairs <- combn(seq_along(lev), 2, simplify = FALSE, FUN = function(i)lev[i])
pval <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("****", "***", "**", "*", "n.s")) 

trans = log_epsilon_trans(epsilon = 0.0001)
pcr %>%
  ggplot(aes(x = Category, y = value, fill = Category))+
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  scale_fill_manual(values = opts$colors)+
  scale_color_manual(values = opts$colors)+
  geom_jitter(aes(color = Category), alpha = 1)+
  theme_bw() +
  ggtitle("16s/18s qPCR") +
  scale_y_continuous(trans = log_epsilon_trans(epsilon = 0.0001))+
  ylab("")+
  xlab("")+ 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.text =element_text(size =12))+
  facet_wrap(.~variable, ncol = 3, scales = "free")+
  stat_compare_means(method = "wilcox.test",
                     comparisons = L.pairs, 
                     label = "p.signif", 
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1), 
                                        symbols = c("****", "***", "**", "*")))+
  stat_summary(fun.data = give.n, geom = "text", size=4)


###########################################

ratio <- read_xlsx("D:/Data/Documents/PhD/Transkingdom interactions ICU/Metadata/final_16s_18s_PCR_TKI.xlsx", sheet = "Blad1")%>%
  #mutate(sample = as.character(sample))%>%
  left_join(metadata)%>%
  #filter(day != "d9") %>%
  mutate(Category = if_else(Category %like% "sepsis", "Sepsis", Category),
         Category = if_else(Category == "healthy_no_abx", "Healthy, no antibiotics", if_else(Category == "non_septic_icu", "Non septic ICU", 
                                                                                             if_else(Category == "healthy_abx" ,"Healthy, antibiotics", Category))))%>%
  select(seqID, Category, corrected_16s, corrected_18s, def_fungal_bacterial_ratio, Butyrate_mg_feces, Acetate_mg_feces, Propionate_mg_feces)


ratio %>%
  ggplot(aes(x = corrected_16s, y = corrected_18s))+
  geom_point(aes(color = Category))+
  geom_smooth(method = "lm")+
  scale_y_continuous(trans = log_epsilon_trans(epsilon = 0.00001))+
  scale_x_continuous(trans = log_epsilon_trans(epsilon = 0.01))+
  theme_bw()+
  geom_label(label = "R = -0.31, p = 0.01782", x= 1.5, y =1)


ratio %>%
  ggplot(aes(x = Butyrate_mg_feces, y = corrected_18s))+
  geom_point(aes(color = Category))+
  geom_smooth(method = "lm")+
  scale_y_continuous(trans = log_epsilon_trans(epsilon = 0.00001))+
  scale_x_continuous(trans = log_epsilon_trans(epsilon = .1))+
  theme_bw()+
  geom_label(label = "R = -0.74, p = 1.573e-11", x= 1.5, y =1)

ratio %>%
  ggplot(aes(x = Acetate_mg_feces, y = corrected_18s))+
  geom_point(aes(color = Category))+
  geom_smooth(method = "lm")+
  scale_y_continuous(trans = log_epsilon_trans(epsilon = 0.00001))+
  scale_x_continuous(trans = log_epsilon_trans(epsilon = 1))+
  theme_bw()

ratio %>%
  ggplot(aes(x = Propionate_mg_feces, y = corrected_18s))+
  geom_point(aes(color = Category))+
  geom_smooth(method = "lm")+
  scale_y_continuous(trans = log_epsilon_trans(epsilon = 0.00001))+
  scale_x_continuous(trans = log_epsilon_trans(epsilon = 1))+
  theme_bw()


cor.test(y=trans$transform(ratio$corrected_16s), x=trans$transform(ratio$corrected_18s))
cor.test(y=trans$transform(ratio$Butyrate_mg_feces), x=trans$transform(ratio$corrected_18s))
cor.test(y=trans$transform(ratio$Propionate_mg_feces), x=trans$transform(ratio$corrected_18s))



