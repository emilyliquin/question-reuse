
library(tidyverse)
library(ggbeeswarm)
library(ggpubr)
library(see)
library(ggridges)
library(rsample)

setwd(this.path::here())


# read data
df <- read_csv("data/dqa_baseline_preppeddata.csv", col_types = cols(prev_trial_indices_reuse = "c",
                                                                     prev_trial_indices_remixing = "c"))

glimpse(df)

df <- df %>% mutate(AgeGroup = as.factor(AgeGroup),
                    id = as.factor(id),
                    trial_index = as.factor(trial_index),
                    AgeGroup_Split = as.factor(AgeGroup_Split))

df$AgeGroup <- relevel(df$AgeGroup, ref = "Kids")


df_noskips <- df %>% filter( notes != "skipped" | is.na(notes))
df_valid <- df_noskips %>% filter(!is.na(question_program))


####### summary of questions #######


# question variety (histogram)

df_valid$AgeGroup <- ifelse(df_valid$AgeGroup == "Kids", "Children", "Adults")

df_valid$question_abstracted_idealized = fct_infreq(as.factor(df_valid$question_abstracted_idealized))

allhist <- ggplot(df_valid) + 
  geom_bar(aes(x = question_abstracted_idealized, 
               y = after_stat(count)/sum(after_stat(count)),
               fill = AgeGroup)) + 
  theme_classic(base_size = 9)+ 
  ylab("Proportion of\nTotal Questions") + 
  scale_x_discrete(labels = c()) + 
  xlab("Unique\nQuestion Type") + 
  scale_fill_manual(values = c("#403F4C", "#E84855"), 
                    name = "Age Group") +
  theme(legend.position = "bottom")
allhist


# informativeness

df_valid$AgeGroup <- factor(df_valid$AgeGroup, levels = c("Children", "Adults"))


# get bootstrap 95% CIs (clustered bootstrap: sample participants, not datapoints)
# see https://www.r-bloggers.com/2018/08/bootstrapping-clustered-data/amp/
# and https://www.r-bloggers.com/2013/01/the-cluster-bootstrap/amp/
df_valid_nest <- df_valid %>% nest(data = -id)
set.seed(154234)
bs <- bootstraps(df_valid_nest, times = 1000)
bs_mEIGs <- map(bs$splits, ~as_tibble(.) %>% unnest(cols = c(data)) %>% 
                  group_by(AgeGroup) %>% 
                  summarize(mEIG = mean(EIG))) %>% 
  bind_rows(.id = 'boots')

cis <- bs_mEIGs %>% group_by(AgeGroup) %>%
  summarize(ci_lo = quantile(mEIG, 0.025),
            ci_hi = quantile(mEIG, 0.975))


p1 <- ggplot() + 
  geom_quasirandom(df_valid, mapping = aes(x = AgeGroup, y = EIG, color = AgeGroup),
                   alpha = 0.05) + 
  stat_summary(data = df_valid, mapping = aes(x = AgeGroup, y = EIG, color = AgeGroup),
               fun.data = "mean_cl_boot", shape = 5, geom = "point") + 
  geom_errorbar(data = cis, mapping = aes(x = AgeGroup, ymin = ci_lo, 
                                          ymax = ci_hi, color = AgeGroup),
                width = 0.1) + 
  theme_classic(base_size = 9) +
  scale_x_discrete(labels = c("Children", "Adults")) + 
  xlab("Age Group") + 
  ylab("Expected Information Gain") + 
  scale_color_manual(values = c("#403F4C", "#E84855"), 
                     name = "Age Group") +
  theme(legend.position = "bottom")
p1


ggarrange(allhist, p1, labels = c("A", "B"), align = "hv", common.legend = TRUE, legend = "bottom") %>%
  ggsave(filename = "figures/Study1_Fig_Combined.pdf", width = 6.5, height = 3, units = "in")


##### reuse and remixing ####

#### reuse rate ###
df_seq_consec <- df %>% filter(!is.na(same_as_last))

sim_reuse_df <- read_csv("simulations/dqa_baseline_reuse.csv")
sim_remixing_df <- read_csv("simulations/dqa_baseline_remixing.csv")

df_seq_consec$AgeGroup <- ifelse(df_seq_consec$AgeGroup == "Kids", "Children", "Adults")
df_seq_consec$AgeGroup <- factor(df_seq_consec$AgeGroup, levels = c("Children", "Adults"))

# get bootstrap 95% CIs (clustered bootstrap: sample participants, not datapoints)
df_nest <- df_seq_consec %>% nest(data = -id)
set.seed(394320)
bs <- bootstraps(df_nest, times = 1000)
bs_summary <- map(bs$splits, ~as_tibble(.) %>% unnest(cols = c(data)) %>% 
                    group_by(AgeGroup) %>% 
                    summarize(mReuse = mean(same_as_last))) %>% 
  bind_rows(.id = 'boots')

cis <- bs_summary %>% group_by(AgeGroup) %>%
  summarize(ci_lo = quantile(mReuse, 0.025),
            ci_hi = quantile(mReuse, 0.975))


reuse_1 <- ggplot(sim_reuse_df) + 
  stat_density_ridges(aes(x = reuse_rate, y = " Trial-matched simulations", fill = factor(stat(quantile))),
                      geom = "density_ridges_gradient",
                      calc_ecdf = TRUE,
                      quantiles = c(0.025, 0.975),
                      scale = 0.022,
                      rel_min_height = .01
  ) +
  scale_fill_manual(
    name = "Probability", values = c("#C2C2C2", "#0000FFA0", "#C2C2C2")
  ) + theme_classic(base_size = 9) +
  stat_summary(data = df_seq_consec, 
               aes(x = same_as_last, y = AgeGroup, color = AgeGroup), 
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
  xlab("Trial-to-Trial Reuse") +
  coord_cartesian(xlim = c(0, 0.6))
reuse_1



df_seq_consec_nonmatch <- df_seq_consec %>% filter(same_as_last == "0")

# get bootstrap 95% CIs (clustered bootstrap: sample participants, not datapoints)
df_nest <- df_seq_consec_nonmatch %>% nest(data = -id)
set.seed(21314)
bs <- bootstraps(df_nest, times = 1000)
bs_summary <- map(bs$splits, ~as_tibble(.) %>% unnest(cols = c(data)) %>% 
                    group_by(AgeGroup) %>% 
                    summarize(mRemixing = mean(sim_to_last_standard, na.rm = TRUE))) %>% 
  bind_rows(.id = 'boots')

cis <- bs_summary %>% group_by(AgeGroup) %>%
  summarize(ci_lo = quantile(mRemixing, 0.025),
            ci_hi = quantile(mRemixing, 0.975))


remixing_1 <- ggplot(sim_remixing_df) + 
  stat_density_ridges(aes(x = remixing_mean, y = " Trial-matched simulations", fill = factor(stat(quantile))),
                      geom = "density_ridges_gradient",
                      calc_ecdf = TRUE,
                      quantiles = c(0.025, 0.975),
                      scale = 0.008,
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
  coord_cartesian(xlim = c(0.63, 0.8))
remixing_1




ggarrange(reuse_1, remixing_1, labels = c("A", "B")) %>%
  ggsave(filename = "figures/Study1_ReuseRemixing.pdf", width = 6.5, height = 2, units = "in")
