

############ ALL FIGURES, BUT BREAK DOWN BY AGE IN CHILDHOOD #############

library(tidyverse)
library(ggbeeswarm)
library(ggpubr)
library(see)
library(ggridges)
library(rsample)

setwd(this.path::here())


#read data
df <- read_csv("data/dqa_exposure_preppeddata.csv", col_types = cols(prev_trial_indices_reuse = "c",
                                                                     prev_trial_indices_remixing = "c"))

glimpse(df)

df <- df %>% mutate(AgeGroup = as.factor(AgeGroup),
                    id = as.factor(id),
                    trial_index = as.factor(trial_index),
                    AgeGroup_Split = as.factor(AgeGroup_Split))

df$AgeGroup <- relevel(df$AgeGroup, ref = "Kids")


df_noskips <- df %>% filter( notes != "skipped" | is.na(notes))
df_valid2 <- df_noskips %>% filter(!is.na(question_program))


##### reuse and remixing #####

df_valid2$AgeGroup_Split <- fct_recode(df_valid2$AgeGroup_Split,
                                       "5- to 6-\nyear-olds" = "5- to 6-year-olds",
                                       "7- to 8-\nyear-olds" = "7- to 8-year-olds",
                                       "9- to 10-\nyear-olds" = "9- to 10-year-olds",
                                       Adults = "Adults")
df_valid2$AgeGroup <- fct_recode(df_valid2$AgeGroup,
                                 Children = "Kids",
                                 Adults = "Adults")

df_valid2$exposure_cond <- factor(df_valid2$exposure_cond, levels = c("exposure", "baseline"))

# get bootstrap 95% CIs (clustered bootstrap: sample participants, not datapoints)
# see https://www.r-bloggers.com/2018/08/bootstrapping-clustered-data/amp/
# and https://www.r-bloggers.com/2013/01/the-cluster-bootstrap/amp/
df_nest <- df_valid2 %>% filter(trial_type != "zero") %>% nest(data = -id)
set.seed(231209)
bs <- bootstraps(df_nest, times = 1000)
bs_summary <- map(bs$splits, ~as_tibble(.) %>% unnest(cols = c(data)) %>% 
                    group_by(AgeGroup_Split, exposure_cond) %>% 
                    summarize(mReuse = mean(target_match))) %>% 
  bind_rows(.id = 'boots')

cis <- bs_summary %>% group_by(AgeGroup_Split, exposure_cond) %>%
  summarize(ci_lo = quantile(mReuse, 0.025),
            ci_hi = quantile(mReuse, 0.975))
cis

p <- ggplot(df_valid2 %>% filter(trial_type != "zero"), aes(x = AgeGroup_Split, color = exposure_cond, group = exposure_cond)) + 
  # geom_bar(aes(fill = factor(target_match)), position = "fill") + 
  # facet_grid(.~AgeGroup_Split) + 
  theme_classic(base_size = 7) + 
  stat_summary(aes(y = as.numeric(as.character(target_match))), 
               fun.data = "mean_cl_boot", shape = 5, size = 2,
               geom= "point", position = position_dodge(0.4)) +
  geom_errorbar(data = cis, mapping = aes(x = AgeGroup_Split, group = exposure_cond, ymin = ci_lo, ymax = ci_hi, 
                                          color = exposure_cond), 
                width = 0.2, position = position_dodge(0.4)) +
  
  ylab("Proportion of Questions\nMatching Target Question") +
  xlab("Age Group") + 
  theme(legend.position = "right") + 
  # scale_fill_manual(values = c("#EECC66", "#6699CC"), 
  #                   name = "Match to Target Question", labels = c("Non-Match", "Match")) + 
  scale_color_manual(values = c("#0000FFA0", "#882255"), 
                     name = "Condition", labels = c("No-Exposure", "Exposure"),
                     breaks = c("baseline", "exposure")
  ) +
  coord_flip(ylim = c(0, 1)) + 
  ggtitle("The Reuse Effect")
p

##### recombination: tree edit distance
## norm
df_valid2$target_dist_normed <- 1-df_valid2$target_dist/max(df_valid2$target_dist, na.rm = T)


df_nonmatch <- subset(df_valid2, df_valid2$target_match != 1)

# get bootstrap 95% CIs (clustered bootstrap: sample participants, not datapoints)
df_nest <- df_nonmatch %>% nest(data = -id)
set.seed(4586465)
bs <- bootstraps(df_nest, times = 1000)
bs_summary <- map(bs$splits, ~as_tibble(.) %>% unnest(cols = c(data)) %>% 
                    group_by(AgeGroup_Split, exposure_cond) %>% 
                    summarize(mRemixing = mean(target_dist_normed))) %>% 
  bind_rows(.id = 'boots')

cis <- bs_summary %>% group_by(AgeGroup_Split, exposure_cond) %>%
  summarize(ci_lo = quantile(mRemixing, 0.025),
            ci_hi = quantile(mRemixing, 0.975))
cis

p2 <- ggplot() + 
  # geom_quasirandom(aes(x = AgeGroup_Split), alpha = 0.1, dodge.width=0.8, size = 0.1) +
  stat_summary(data = df_nonmatch, mapping = aes(x = AgeGroup_Split, y = target_dist_normed, color = exposure_cond, group = exposure_cond),
               fun.data = "mean_cl_boot",  shape = 5, size = 2,
               geom= "point", position = position_dodge(0.4)) + 
  geom_errorbar(data = cis, mapping = aes(x = AgeGroup_Split, group = exposure_cond, color = exposure_cond, ymin = ci_lo, ymax = ci_hi), 
                width = 0.2, position = position_dodge(0.4)) +
  theme_classic(base_size = 7) + 
  ylab("Grammar-Based Similarity\nto Target Question") +
  xlab("") + 
  theme(
    legend.position = "right") +
  scale_color_manual(values = c("#0000FFA0", "#882255"), 
                     name = "Condition", labels = c("No-Exposure", "Exposure"),
                     breaks = c("baseline", "exposure")
  ) +
  coord_flip(ylim = c(0.65, 0.9)) + xlab("Age Group") + 
  ggtitle("The Recombination Effect\n(Grammar-Based Similarity)")

p2



##### recombination: semantic similarity
# get bootstrap 95% CIs (clustered bootstrap: sample participants, not datapoints)
df_nest <- df_nonmatch %>% nest(data = -id)
set.seed(238921)
bs <- bootstraps(df_nest, times = 1000)
bs_summary <- map(bs$splits, ~as_tibble(.) %>% unnest(cols = c(data)) %>% 
                    group_by(AgeGroup_Split, exposure_cond) %>% 
                    summarize(mRemixing = mean(target_sim_standard))) %>% 
  bind_rows(.id = 'boots')

cis <- bs_summary %>% group_by(AgeGroup_Split, exposure_cond) %>%
  summarize(ci_lo = quantile(mRemixing, 0.025),
            ci_hi = quantile(mRemixing, 0.975))
cis

p3 <- ggplot() + 
  # geom_quasirandom(aes(x = AgeGroup_Split), alpha = 0.1, dodge.width=0.8, size = 0.1) +
  stat_summary(data = df_nonmatch, mapping = aes(x = AgeGroup_Split, y = target_sim_standard, color = exposure_cond, group = exposure_cond),
               fun.data = "mean_cl_boot",  shape = 5, size = 2,
               geom= "point", position = position_dodge(0.4)) + 
  geom_errorbar(data = cis, mapping = aes(x = AgeGroup_Split, group = exposure_cond, color = exposure_cond, ymin = ci_lo, ymax = ci_hi), 
                width = 0.2, position = position_dodge(0.4)) +
  theme_classic(base_size = 7) + 
  ylab("Text-Based Similarity\nto Target Question") +
  xlab("") + 
  theme(
    legend.position = "right") +
  scale_color_manual(values = c("#0000FFA0", "#882255"), 
                     name = "Condition", labels = c("No-Exposure", "Exposure"),
                     breaks = c("baseline", "exposure")
  ) +
  coord_flip(ylim = c(0.60, 0.75)) + xlab("Age Group") + 
  ggtitle("The Recombination Effect\n(Text-Based Similarity)")

p3

library(patchwork)
((p / plot_spacer()/ (p2 + p3)) + plot_layout(guides = 'collect', heights = c(8, 1, 8))) %>%
  ggsave(filename = "figures/Study2_ReuseRemixing_Supplement.pdf", width = 5.5, height = 4, units = "in")


###### reuse sensitivity to context ######

### reuse across four trial types
df_valid2$trial_type <- fct_recode(df_valid2$trial_type, 
                                   best = "best",
                                   worst = "zero",
                                   medium = "nonbest",
                                   `too-complex` = "overcomplex")

df_valid2$trial_type <- factor(df_valid2$trial_type,
                               levels = c("best", "medium", "worst", "too-complex"))


# get bootstrap 95% CIs (clustered bootstrap: sample participants, not datapoints)
df_nest <- df_valid2 %>% filter(exposure_cond == "exposure") %>% nest(data = -id)
set.seed(123241)
bs <- bootstraps(df_nest, times = 1000)
bs_summary <- map(bs$splits, ~as_tibble(.) %>% unnest(cols = c(data)) %>% 
                    group_by(AgeGroup_Split, trial_type) %>% 
                    summarize(mReuse = mean(target_match))) %>% 
  bind_rows(.id = 'boots')

cis <- bs_summary %>% group_by(AgeGroup_Split, trial_type) %>%
  summarize(ci_lo = quantile(mReuse, 0.025),
            ci_hi = quantile(mReuse, 0.975))
cis



p3 <- ggplot(df_valid2 %>% filter(exposure_cond == "exposure"), aes(x = trial_type)) + 
  facet_wrap(~AgeGroup_Split, scales = "free") +
  theme_classic(base_size = 7) + 
  stat_summary(aes(y = as.numeric(as.character(target_match))), 
               fun.data = "mean_cl_boot", geom = "point", color = "#882255",
               shape = 5, size = 2) +
  geom_errorbar(data = cis, mapping = aes(x = trial_type, ymin = ci_lo, ymax = ci_hi), 
                width = 0.2, position = position_dodge(0.9), color = "#882255") +
  ylab("Proportion of Questions\nMatching Target Question") +
  xlab("Trial Type (Within Exposure Condition)") + 
  theme(legend.position = "top") + ylim(0, 1) + 
  coord_flip(ylim = c(0, 0.65)) +
  ggtitle("Context-Specific Variation in Reuse")
p3


ggsave(p3, filename = "figures/Study2_ReuseContext_Supplement.pdf", width = 5.5, height = 3, units = "in")
