library(tidyverse)
library(ggbeeswarm)
library(ggpubr)
library(see)
library(ggridges)

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

p <- ggplot(df_valid2 %>% filter(trial_type != "zero"), aes(x = AgeGroup, fill = exposure_cond, group = exposure_cond)) + 
  # geom_bar(aes(fill = factor(target_match)), position = "fill") + 
  # facet_grid(.~AgeGroup) + 
  theme_classic(base_size = 9) + 
  stat_summary(aes(y = as.numeric(as.character(target_match))), 
               fun.data = "mean_cl_boot", geom = "bar", position = position_dodge(0.9)) +
  geom_errorbar(data = cis, mapping = aes(x = AgeGroup, group = exposure_cond, ymin = ci_lo, ymax = ci_hi), 
                width = 0.2, position = position_dodge(0.9), color = "black") +
  
  ylab("Proportion of Questions\nMatching Target") +
  xlab("Age Group") + 
  theme(legend.position = "top") + 
  # scale_fill_manual(values = c("#EECC66", "#6699CC"), 
  #                   name = "Match to Target Question", labels = c("Non-Match", "Match")) + 
  scale_fill_manual(values = c("#F9DC5C", "#7D8570"), 
                    name = "Condition", labels = c("No-Exposure", "Exposure")) +
  ylim(0, 1)+
  coord_cartesian(ylim = c(0, 0.35))
p

df_nonmatch <- subset(df_valid2, df_valid2$target_match != 1)

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

p2 <- ggplot() + 
  # geom_quasirandom(aes(x = AgeGroup), alpha = 0.1, dodge.width=0.8, size = 0.1) +
  stat_summary(data = df_nonmatch, mapping = aes(x = AgeGroup, y = target_sim_standard, fill = exposure_cond, group = exposure_cond),
               fun.data = "mean_cl_boot", geom = "bar", position = position_dodge(0.9), size = 3) + 
  geom_errorbar(data = cis, mapping = aes(x = AgeGroup, group = exposure_cond, ymin = ci_lo, ymax = ci_hi), 
                width = 0.2, position = position_dodge(0.9), color = "black") +
  theme_classic(base_size = 9) + 
  ylab("Similarity to\nTarget Question") +
  xlab("") + 
  theme(
    legend.position = "right") +
  scale_x_discrete(breaks = c("Children", "Adults"), labels = c("Children", "Adults")) + 
  scale_fill_manual(values = c("#F9DC5C", "#7D8570"), 
                    name = "Condition", labels = c("No-Exposure", "Exposure")) +
  coord_cartesian(ylim = c(0.62, 0.72)) + xlab("Age Group")
p2

ggarrange(p, p2, labels = c("A", "B"), align = "hv", common.legend = TRUE, legend = "top") %>%
  ggsave(filename = "figures/Study2_ReuseRemixing.pdf", width = 6.5, height = 3, units = "in")


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
  facet_grid(.~AgeGroup) +
  theme_classic(base_size = 9) + 
  stat_summary(aes(y = as.numeric(as.character(target_match))), 
               fun.data = "mean_cl_boot", geom = "bar", fill = "#7D8570") +
  geom_errorbar(data = cis, mapping = aes(x = trial_type, ymin = ci_lo, ymax = ci_hi), 
                width = 0.2, position = position_dodge(0.9), color = "black") +
  ylab("Proportion of Questions\nMatching Target") +
  xlab("Trial Type (Within Exposure Condition)") + 
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1),
        legend.position = "top") + ylim(0, 1) + 
  coord_cartesian(ylim = c(0, 0.65))
p3


ggsave(p3, filename = "figures/Study2_ReuseContext.pdf", width = 6.5, height = 3, units = "in")


###### EIG #####

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
  theme_classic(base_size = 9) +
  scale_x_discrete(labels = c("Children", "Adults")) + 
  xlab("Age Group") + 
  ylab("Expected Information Gain") +
  scale_color_manual(values = c("#F9DC5C", "#7D8570"), 
                     name = "Condition", labels = c("No-Exposure", "Exposure")) + 
  theme(legend.position = "top")
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
  stat_density_ridges(aes(x = reuse_rate, y = " Trial-matched simulations", fill = factor(stat(quantile))),
                      geom = "density_ridges_gradient",
                      calc_ecdf = TRUE,
                      quantiles = c(0.025, 0.975),
                      scale = 0.025,
                      rel_min_height = .01
  ) +
  scale_fill_manual(
    name = "Probability", values = c("#C2C2C2", "#0000FFA0", "#C2C2C2")
  ) + theme_classic(base_size = 9) +
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
  ylab("") + 
  xlab("Trial-to-Trial Reuse") +
  coord_cartesian(xlim = c(0, 0.6))
reuse_1




df_seq_consec_nonmatch <- df_seq_consec %>% filter(same_as_last == "0")


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


hist(sim_remixing_df$remixing_mean)

remixing_1 <- ggplot(sim_remixing_df) + 
  stat_density_ridges(aes(x = remixing_mean, y = " Trial-matched simulations", fill = factor(stat(quantile))),
                      geom = "density_ridges_gradient",
                      calc_ecdf = TRUE,
                      quantiles = c(0.025, 0.975),
                      scale = 0.005,
                      rel_min_height = .01
  ) +
  scale_fill_manual(
    name = "Probability", values = c("#C2C2C2", "#0000FFA0", "#C2C2C2")
  ) + theme_classic(base_size = 9) +
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
  xlab("Trial-to-Trial Remixing") +
  coord_cartesian(xlim = c(0.65, 0.78))
remixing_1



ggarrange(reuse_1, remixing_1, labels = c("A", "B")) %>%
  ggsave(filename = "figures/Study2_ReuseRemixing_Trials.pdf", width = 6.5, height = 2.5, units = "in")

