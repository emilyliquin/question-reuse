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
                    group_by(AgeGroup, exposure_cond) %>% 
                    summarize(mReuse = mean(target_match))) %>% 
  bind_rows(.id = 'boots')

cis <- bs_summary %>% group_by(AgeGroup, exposure_cond) %>%
  summarize(ci_lo = quantile(mReuse, 0.025),
            ci_hi = quantile(mReuse, 0.975))
cis

p <- ggplot(df_valid2 %>% filter(trial_type != "zero"), aes(x = AgeGroup, color = exposure_cond, group = exposure_cond)) + 
  # geom_bar(aes(fill = factor(target_match)), position = "fill") + 
  # facet_grid(.~AgeGroup) + 
  theme_classic(base_size = 7) + 
  stat_summary(aes(y = as.numeric(as.character(target_match))), 
               fun.data = "mean_cl_boot", shape = 5, size = 2,
               geom= "point", position = position_dodge(0.4)) +
  geom_errorbar(data = cis, mapping = aes(x = AgeGroup, group = exposure_cond, ymin = ci_lo, ymax = ci_hi, 
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
  coord_flip(ylim = c(0, 0.4)) + 
  ggtitle("The Reuse Effect")
p

##### recombination: tree edit distance
df_nonmatch <- subset(df_valid2, df_valid2$target_match != 1)

# get bootstrap 95% CIs (clustered bootstrap: sample participants, not datapoints)
df_nest <- df_nonmatch %>% nest(data = -id)
set.seed(4586465)
bs <- bootstraps(df_nest, times = 1000)
bs_summary <- map(bs$splits, ~as_tibble(.) %>% unnest(cols = c(data)) %>% 
                    group_by(AgeGroup, exposure_cond) %>% 
                    summarize(mRemixing = mean(target_dist))) %>% 
  bind_rows(.id = 'boots')

cis <- bs_summary %>% group_by(AgeGroup, exposure_cond) %>%
  summarize(ci_lo = quantile(mRemixing, 0.025),
            ci_hi = quantile(mRemixing, 0.975))
cis

p2 <- ggplot() + 
  # geom_quasirandom(aes(x = AgeGroup), alpha = 0.1, dodge.width=0.8, size = 0.1) +
  stat_summary(data = df_nonmatch, mapping = aes(x = AgeGroup, y = target_dist, color = exposure_cond, group = exposure_cond),
               fun.data = "mean_cl_boot",  shape = 5, size = 2,
               geom= "point", position = position_dodge(0.4)) + 
  geom_errorbar(data = cis, mapping = aes(x = AgeGroup, group = exposure_cond, color = exposure_cond, ymin = ci_lo, ymax = ci_hi), 
                width = 0.2, position = position_dodge(0.4)) +
  theme_classic(base_size = 7) + 
  ylab("Tree Edit Distance\nto Target Question") +
  xlab("") + 
  theme(
    legend.position = "right") +
  scale_x_discrete(breaks = c("Children", "Adults"), labels = c("Children", "Adults")) + 
  scale_color_manual(values = c("#0000FFA0", "#882255"), 
                     name = "Condition", labels = c("No-Exposure", "Exposure"),
                     breaks = c("baseline", "exposure")
  ) +
  coord_flip(ylim = c(4, 11)) + xlab("Age Group") + 
  ggtitle("The Recombination Effect\n(Tree Edit Distance)")

p2



##### recombination: semantic similarity
# get bootstrap 95% CIs (clustered bootstrap: sample participants, not datapoints)
df_nest <- df_nonmatch %>% nest(data = -id)
set.seed(238921)
bs <- bootstraps(df_nest, times = 1000)
bs_summary <- map(bs$splits, ~as_tibble(.) %>% unnest(cols = c(data)) %>% 
                    group_by(AgeGroup, exposure_cond) %>% 
                    summarize(mRemixing = mean(target_sim_standard))) %>% 
  bind_rows(.id = 'boots')

cis <- bs_summary %>% group_by(AgeGroup, exposure_cond) %>%
  summarize(ci_lo = quantile(mRemixing, 0.025),
            ci_hi = quantile(mRemixing, 0.975))
cis

p3 <- ggplot() + 
  # geom_quasirandom(aes(x = AgeGroup), alpha = 0.1, dodge.width=0.8, size = 0.1) +
  stat_summary(data = df_nonmatch, mapping = aes(x = AgeGroup, y = target_sim_standard, color = exposure_cond, group = exposure_cond),
               fun.data = "mean_cl_boot",  shape = 5, size = 2,
               geom= "point", position = position_dodge(0.4)) + 
  geom_errorbar(data = cis, mapping = aes(x = AgeGroup, group = exposure_cond, color = exposure_cond, ymin = ci_lo, ymax = ci_hi), 
                width = 0.2, position = position_dodge(0.4)) +
  theme_classic(base_size = 7) + 
  ylab("Text-Based Semantic Similarity\nto Target Question") +
  xlab("") + 
  theme(
    legend.position = "right") +
  scale_x_discrete(breaks = c("Children", "Adults"), labels = c("Children", "Adults")) + 
  scale_color_manual(values = c("#0000FFA0", "#882255"), 
                     name = "Condition", labels = c("No-Exposure", "Exposure"),
                     breaks = c("baseline", "exposure")
  ) +
  coord_flip(ylim = c(0.62, 0.72)) + xlab("Age Group") + 
  ggtitle("The Recombination Effect\n(Text-Based Similarity)")

p3

library(patchwork)
((p / plot_spacer()/ (p2 + p3)) + plot_layout(guides = 'collect', heights = c(8, 1, 8))) %>%
  ggsave(filename = "figures/Study2_ReuseRemixing.pdf", width = 5.5, height = 4, units = "in")


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
                    group_by(AgeGroup, trial_type) %>% 
                    summarize(mReuse = mean(target_match))) %>% 
  bind_rows(.id = 'boots')

cis <- bs_summary %>% group_by(AgeGroup, trial_type) %>%
  summarize(ci_lo = quantile(mReuse, 0.025),
            ci_hi = quantile(mReuse, 0.975))
cis



p3 <- ggplot(df_valid2 %>% filter(exposure_cond == "exposure"), aes(x = trial_type)) + 
  facet_wrap(~AgeGroup, scales = "free") +
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


ggsave(p3, filename = "figures/Study2_ReuseContext.pdf", width = 5.5, height = 2.5, units = "in")


###### EIG #####

df_valid2$exposure_cond <- factor(df_valid2$exposure_cond, levels = c("baseline", "exposure"))

# get bootstrap 95% CIs (clustered bootstrap: sample participants, not datapoints)
df_valid_nest <- df_valid2 %>% nest(data = -id)
set.seed(423422)
bs <- bootstraps(df_valid_nest, times = 1000)
bs_mEIGs <- map(bs$splits, ~as_tibble(.) %>% unnest(cols = c(data)) %>% 
                  group_by(AgeGroup, exposure_cond) %>% 
                  summarize(mEIG = mean(EIG))) %>% 
  bind_rows(.id = 'boots')

cis <- bs_mEIGs %>% group_by(AgeGroup, exposure_cond) %>%
  summarize(ci_lo = quantile(mEIG, 0.025),
            ci_hi = quantile(mEIG, 0.975))



p2 <- ggplot(df_valid2) + 
  geom_quasirandom(mapping = aes(x = AgeGroup, y = EIG, color = exposure_cond),
                   alpha = 0.03, dodge.width = 0.7) + 
  stat_summary(mapping = aes(x = AgeGroup, y = EIG, color = exposure_cond), 
               fun.data = "mean_cl_boot", shape = 5, position = position_dodge(0.7), geom = "point") + 
  geom_errorbar(data = cis, mapping = aes(x = AgeGroup, ymin = ci_lo, 
                                          ymax = ci_hi, color = exposure_cond),
                width = 0.1, position = position_dodge(0.7)) + 
  theme_classic(base_size = 7) +
  scale_x_discrete(labels = c("Children", "Adults")) + 
  xlab("Age Group") + 
  ylab("Expected Information Gain") +
  scale_color_manual(values = c("#0000FFA0","#882255"),
                     name = "Condition", labels = c("No-Exposure", "Exposure")) +
  theme(legend.position = "top") + 
  ggtitle("Question Informativeness")
p2

ggsave(p2, filename = "figures/Study2_EIG.pdf", width = 3.5, height = 3, units = "in")



###### trial-to-trial reuse/remixing ####

#### reuse rate ###


df_seq_consec <- df %>% filter(!is.na(same_as_last))

sim_reuse_df <- read_csv("simulations/dqa_exposure_reuse.csv")
sim_remixing_df <- read_csv("simulations/dqa_exposure_remixing.csv")


df_seq_consec$AgeGroup <- ifelse(df_seq_consec$AgeGroup == "Kids", "Children", "Adults")
df_seq_consec$AgeGroup <- factor(df_seq_consec$AgeGroup, levels = c("Children", "Adults"))


# get bootstrap 95% CIs (clustered bootstrap: sample participants, not datapoints)
df_nest <- df_seq_consec %>% nest(data = -id)
set.seed(209650)
bs <- bootstraps(df_nest, times = 1000)
bs_summary <- map(bs$splits, ~as_tibble(.) %>% unnest(cols = c(data)) %>% 
                    group_by(AgeGroup_Split) %>% 
                    summarize(mReuse = mean(same_as_last))) %>% 
  bind_rows(.id = 'boots')

cis <- bs_summary %>% group_by(AgeGroup_Split) %>%
  summarize(ci_lo = quantile(mReuse, 0.025),
            ci_hi = quantile(mReuse, 0.975))



reuse_1 <- ggplot(sim_reuse_df) + 
  stat_density_ridges(aes(x = reuse_rate, y = "Simulated null\ndistribution", fill = factor(stat(quantile))),
                      geom = "density_ridges_gradient",
                      calc_ecdf = TRUE,
                      quantiles = c(0.025, 0.975),
                      scale = 0.022,
                      rel_min_height = .01
  ) +
  scale_fill_manual(
    name = "Probability", values = c("#C2C2C2", "#0000FFA0", "#C2C2C2")
  ) + theme_classic(base_size = 7) +
  stat_summary(data = df_seq_consec, 
               aes(x = same_as_last, y = AgeGroup_Split, color = AgeGroup_Split), 
               fun.data = "mean_cl_boot", 
               shape = 5, size = 2,
               geom= "point") +
  geom_errorbar(data = cis, mapping = aes(y = AgeGroup_Split, xmin = ci_lo, 
                                          xmax = ci_hi, color = AgeGroup_Split),
                width = 0.1) +
  scale_color_manual(values = c("#9190A2", "#67667A", "#403F4C", "#E84855"), 
                     name = "Age Group") + 
  theme(legend.position = "none") + 
  ylab("Group") + 
  xlab("Proportion of Questions\nMatching a Previous Question") +
  coord_cartesian(xlim = c(0, 0.6)) + 
  ggtitle("Across-Trial Reuse")
reuse_1




df_seq_consec_nonmatch <- df_seq_consec %>% filter(same_as_last == "0")

##### tree edit distance
df_nest <- df_seq_consec_nonmatch %>% nest(data = -id)
set.seed(45465)
bs <- bootstraps(df_nest, times = 1000)
bs_summary <- map(bs$splits, ~as_tibble(.) %>% unnest(cols = c(data)) %>% 
                    group_by(AgeGroup_Split) %>% 
                    summarize(mRemixing = mean(dist_to_last, na.rm = TRUE))) %>% 
  bind_rows(.id = 'boots')

cis <- bs_summary %>% group_by(AgeGroup_Split) %>%
  summarize(ci_lo = quantile(mRemixing, 0.025),
            ci_hi = quantile(mRemixing, 0.975))


remixing_1 <- ggplot(sim_remixing_df) + 
  stat_density_ridges(aes(x = treedist_mean, y = "Simulated null\ndistribution", fill = factor(stat(quantile))),
                      geom = "density_ridges_gradient",
                      calc_ecdf = TRUE,
                      quantiles = c(0.025, 0.975),
                      scale = 0.5,
                      rel_min_height = .01
  ) +
  scale_fill_manual(
    name = "Probability", values = c("#C2C2C2", "#0000FFA0", "#C2C2C2")
  ) + theme_classic(base_size = 7) +
  stat_summary(data = df_seq_consec_nonmatch, 
               aes(x = dist_to_last, y = AgeGroup_Split, color = AgeGroup_Split), 
               fun.data = "mean_cl_boot", 
               shape = 5, size = 2,
               geom= "point") +
  geom_errorbar(data = cis, mapping = aes(y = AgeGroup_Split, xmin = ci_lo, 
                                          xmax = ci_hi, color = AgeGroup_Split),
                width = 0.1) +
  scale_color_manual(values = c("#9190A2", "#67667A", "#403F4C", "#E84855"), 
                     name = "Age Group") + 
  theme(legend.position = "none") + 
  ylab("Group") + 
  xlab("Tree Edit Distance to\nMost-Similar Previous Question") +
  coord_cartesian(xlim = c(0, 11)) +
  ggtitle("Across-Trial Recombination\n(Tree Edit Distance)")
remixing_1

#### text-based semantic sim

# get bootstrap 95% CIs (clustered bootstrap: sample participants, not datapoints)
df_nest <- df_seq_consec_nonmatch %>% nest(data = -id)
set.seed(391023)
bs <- bootstraps(df_nest, times = 1000)
bs_summary <- map(bs$splits, ~as_tibble(.) %>% unnest(cols = c(data)) %>% 
                    group_by(AgeGroup) %>% 
                    summarize(mRemixing = mean(sim_to_last_standard, na.rm = TRUE))) %>% 
  bind_rows(.id = 'boots')

cis <- bs_summary %>% group_by(AgeGroup) %>%
  summarize(ci_lo = quantile(mRemixing, 0.025),
            ci_hi = quantile(mRemixing, 0.975))


hist(sim_remixing_df$textsim_mean)

remixing_2 <- ggplot(sim_remixing_df) + 
  stat_density_ridges(aes(x = textsim_mean, y = "Simulated null\ndistribution", fill = factor(stat(quantile))),
                      geom = "density_ridges_gradient",
                      calc_ecdf = TRUE,
                      quantiles = c(0.025, 0.975),
                      scale = 0.008,
                      rel_min_height = .01
  ) +
  scale_fill_manual(
    name = "Probability", values = c("#C2C2C2", "#0000FFA0", "#C2C2C2")
  ) + theme_classic(base_size = 7) +
  stat_summary(data = df_seq_consec_nonmatch, 
               aes(x = sim_to_last_standard, y = AgeGroup, color = AgeGroup), 
               fun.data = "mean_cl_boot", 
               shape = 5, size = 2,
               geom= "point") +
  geom_errorbar(data = cis, mapping = aes(y = AgeGroup, xmin = ci_lo, 
                                          xmax = ci_hi, color = AgeGroup),
                width = 0.1) +
  scale_color_manual(values = c("#403F4C", "#E84855"), 
                     name = "Age Group") + 
  theme(legend.position = "none") + 
  ylab("") + 
  xlab("Text-Based Semantic Similarity to\nMost-Similar Previous Question") +
  coord_cartesian(xlim = c(0.63, 0.8)) + 
  ggtitle("Across-Trial Recombination\n(Text-Based Similarity)")
remixing_2


((reuse_1 / plot_spacer()/  (remixing_1 + remixing_2)) + plot_layout(heights = c(8, 1, 8))) %>%
  ggsave(filename = "figures/Study2_ReuseRemixing_Trials.pdf", width = 5.5, height = 4, units = "in")


