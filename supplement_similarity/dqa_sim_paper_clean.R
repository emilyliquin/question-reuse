library(tidyverse)
library(patchwork)

setwd(this.path::here())
fulldf <- read_csv("data/dqa_similarity_preppeddata.csv")


#### remove people who didn't pass attention check  #####
fulldf <- fulldf %>% filter(attn1 == 1 & attn2 == 1)


#### summarize by-pair human-rated similarity ####
fulldf_byitem <- fulldf %>% group_by(pair, sim_text, dist_tree, sim_text_standard, 
                                     intersecting_arguments12, intersecting_arguments21,
                                     intersecting_functions12, intersecting_functions21) %>%
  summarize(msim = mean(similarity))

#### plot scatter plots for 7 similarity measures #####
make_scatter <- function(modelvar, modellab){
  p1 <- ggplot(fulldf_byitem, aes(x = {{modelvar}}, y = msim)) + 
    geom_point(size = 1) + 
    geom_smooth(method = "lm") + 
    theme_classic(base_size = 9) + 
    xlab(modellab) + 
    ylab("Mean human-\nrated similarity")
  return(p1)
}

p1 <- make_scatter(sim_text, "Original text similarity")
p2 <- make_scatter(sim_text_standard, "Standardized text similarity")
p3 <- make_scatter(dist_tree, "Tree edit distance")
p4 <- make_scatter(intersecting_arguments12, "Intersecting arguments (order 1)")
p5 <- make_scatter(intersecting_arguments21, "Intersecting arguments (order 2)")
p6 <- make_scatter(intersecting_functions12, "Intersecting functions (order 1)")
p7 <- make_scatter(intersecting_functions21, "Intersecting functions (order 2)")


p_all <- (plot_spacer() + p3 + plot_spacer()+ plot_layout(widths = c(0.7, 2, 1.3))) / 
  (p2+p1) / 
  (p4+p5) / 
  (p6+p7)
p_all

# ggsave(p_all, filename = "figures/sim_scatterplots.pdf", width = 5, height = 5, units = "in")


#### get correlations for 7 similarity measures #####
cor.test(fulldf_byitem$sim_text, fulldf_byitem$msim)
cor.test(fulldf_byitem$dist_tree, fulldf_byitem$msim)
cor.test(fulldf_byitem$sim_text_standard, fulldf_byitem$msim)
cor.test(fulldf_byitem$intersecting_arguments12, fulldf_byitem$msim)
cor.test(fulldf_byitem$intersecting_arguments21, fulldf_byitem$msim)
cor.test(fulldf_byitem$intersecting_functions12, fulldf_byitem$msim)
cor.test(fulldf_byitem$intersecting_functions21, fulldf_byitem$msim)

