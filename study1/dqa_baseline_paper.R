## ----load data---------------------------------------------------------------------------------------------

library(tidyverse)
library(lme4)
library(papaja)
library(stargazer)
library(car)
library(broom.mixed)


setwd(this.path::here())
df <- read_csv("data/dqa_baseline_preppeddata.csv", col_types = cols(prev_trial_indices_reuse = "c",
                                                                           prev_trial_indices_remixing = "c"))


df <- df %>% mutate(AgeGroup = as.factor(AgeGroup),
                    id = as.factor(id),
                    trial_index = as.factor(trial_index),
                    AgeGroup_Split = as.factor(AgeGroup_Split))

df$AgeGroup <- relevel(df$AgeGroup, ref = "Kids")



## ----study 1 participants----------------------------------------------------------------------------------

n_parts <- df %>% group_by(AgeGroup) %>%
  summarize(n = n()/4)

ages <- df %>% filter(AgeGroup == "Kids") %>%
  group_by(age_yr) %>%
  summarize(n = n()/4)

n_parts
ages

## ----remove everything but valid questions-------------------------------------------------------------------------------------------------

total_trials_kids <- df %>% filter(AgeGroup == "Kids") %>%
  nrow()

total_trials_adults <- df %>% filter(AgeGroup == "Adults") %>%
  nrow()


n_skip_kids <- sum(df[df$AgeGroup == "Kids", "notes"] == "skipped" | df[df$AgeGroup == "Kids", "notes"] == "bad audio", na.rm = T)
n_skip_adults <- sum(df[df$AgeGroup == "Adults", "notes"] == "skipped" | df[df$AgeGroup == "Adults", "notes"] == "bad audio", na.rm = T)

n_skip_kids
n_skip_adults


#valid and ambiguous


n_invalid_kids <- sum(df[df$AgeGroup == "Kids", "notes"] == "invalid", na.rm = T)
n_invalid_adults <- sum(df[df$AgeGroup == "Adults", "notes"] == "invalid", na.rm = T)
n_invalid_kids/(total_trials_kids-n_skip_kids)
n_invalid_adults/(total_trials_adults-n_skip_adults)


n_ambig_kids <- sum(df[df$AgeGroup == "Kids", "notes"] == "ambiguous", na.rm = T)
n_ambig_adults <- sum(df[df$AgeGroup == "Adults", "notes"] == "ambiguous", na.rm = T)
n_ambig_kids/(total_trials_kids-n_skip_kids)
n_ambig_adults/(total_trials_adults-n_skip_adults)

n_gram_kids <- sum(df[df$AgeGroup == "Kids", "notes"] == "not in grammar", na.rm = T)
n_gram_adults <- sum(df[df$AgeGroup == "Adults", "notes"] == "not in grammar", na.rm = T)
n_gram_kids
n_gram_adults
n_gram_adults/(total_trials_adults-n_skip_adults-n_invalid_adults-n_ambig_adults)


##### for supplement: descriptives about excluded trials ####

df$trouble_response <- as.factor(ifelse(is.na(df$question_program), "yes", "no"))

by_part_trials <- df %>% group_by(id, AgeGroup) %>%
  summarize(n_valid_trials = sum(trouble_response == "no"))

table(by_part_trials$AgeGroup, by_part_trials$n_valid_trials)

excl.mod.kid <- glmer(trouble_response ~ age_mo_center + (1|id) + (1|trial_index), 
             data = df %>% filter(AgeGroup == "Kids"),
             family = "binomial")
summary(excl.mod.kid)
anova(excl.mod.kid, update(excl.mod.kid, . ~ . - age_mo_center)) ## no effect of age in kids

excl.mod.all <- glmer(trouble_response ~ AgeGroup + (1|id) + (1|trial_index), 
              data = df,
              family = "binomial")
summary(excl.mod.all)
Confint(excl.mod.all, exponentiate = TRUE)
anova(excl.mod.all, update(excl.mod.all, . ~ . - AgeGroup))



#just look at valid and coded questions from here on out
df_valid <- df %>% filter(!is.na(question_program))

summary_final_sample <- df_valid %>% group_by(AgeGroup) %>%
  summarize(n_part = length(unique(id)),
            n_q = n())
summary_final_sample


## ----question variety--------------------------------------------------------------------------------------

## for each trial, get number of total questions (how many participants did that trial)
# and number of unique questions
qspertrial <- df_valid %>% group_by(trial_index, AgeGroup) %>%
  summarize(unique_qs = length(unique(question_abstracted_idealized)),
            total_qs = n())

# proportion of total questions that are unique
qspertrial$p_unique <- qspertrial$unique_qs/qspertrial$total_qs

qspertrial %>% group_by(AgeGroup) %>% summarize(m = mean(p_unique))

# is there a significant difference by age group?
apa_print(t.test(p_unique~AgeGroup, data = qspertrial, var.equal = T))

# how many total unique questions across all trials?
df_valid %>%
  group_by(AgeGroup) %>%
  summarize(n_unique = length(unique(question_abstracted_idealized)))


## ----question complexity---------------------------------------------------------------------


#### number of unique functions

df_valid %>% 
  group_by(AgeGroup) %>%
  reframe(range = range(n_unique_operations),
          m = mean(n_unique_operations),
          sd = sd(n_unique_operations))

#### within kids
unique.kid <- lmer(n_unique_operations ~ age_mo_center + (1|id) + (1|trial_index), 
             data = df_valid %>% filter(AgeGroup == "Kids"))
summary(unique.kid)
anova(unique.kid, update(unique.kid, . ~ . - age_mo_center)) 
### significant effect - split age group for main analysis

#### kids (split) vs. adults
unique.all <- lmer(n_unique_operations ~ AgeGroup_Split + (1|id) + (1|trial_index), 
             data = as.data.frame(df_valid))
summary(unique.all)
anova(unique.all, update(unique.all, . ~ . - AgeGroup_Split))
Confint(unique.all)


stargazer(unique.all, type = "text",
          dep.var.labels = "Number of unique functions",
          ci = TRUE, digits = 2, digits.extra = 3, single.row = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001), omit.stat = c("aic", "bic", "ll", "n"))



#### total number of functions

df_valid %>% 
  group_by(AgeGroup) %>%
  reframe(range = range(n_operations),
          m = mean(n_operations),
          sd = sd(n_operations))

### within kids
total.kid <- lmer(n_operations ~ age_mo_center + (1|id) + (1|trial_index), 
             data = df_valid %>% filter(AgeGroup == "Kids"))
summary(total.kid)
anova(total.kid, update(total.kid, . ~ . - age_mo_center)) 
# non-significant effect - use kids vs. adults for main analysis

### kids (together) vs. adults
total.all <- lmer(n_operations ~ AgeGroup + (1|id) + (1|trial_index), 
             data = df_valid)
summary(total.all)
anova(total.all, update(total.all, . ~ . - AgeGroup)) # non-significant effect
Confint(total.all)


## ----EIG---------------------------------------------------------------------------------------------------


df_valid %>% 
  group_by(AgeGroup) %>%
  reframe(range = range(EIG),
          m = mean(EIG),
          sd = sd(EIG))


### just within kids
eig.kid <- lmer(EIG ~ age_mo_center + (1|id) + (1|trial_index), 
             data = df_valid %>% filter(AgeGroup == "Kids"))
summary(eig.kid)
anova(eig.kid, update(eig.kid, . ~ . - age_mo_center)) 
# no effect - just kids vs. adults for main analysis

### kids vs. adults
eig.all <- lmer(EIG ~ AgeGroup + (1|id) + (1|trial_index), 
             data = df_valid)
summary(eig.all)
anova(eig.all, update(eig.all, . ~ .- AgeGroup))
Confint(eig.all)


## ----max expected EIG---------------------------------------------------------------------

# if best question (full entropy reduction) was asked on each trial, what would EIG be?
mean(df_valid$entropy)

# actual average EIG
mean(df_valid$EIG)


# most informative question actually asked on each trial
best_Qs <- df_valid %>% group_by(trial_index) %>%
  summarize(max_EIG = max(EIG),
            entropy = max(entropy))
best_Qs

# average best EIG
mean(best_Qs$max_EIG)


## ----context-sensitivity---------------------------------------------------------------------

sim_context_df <- read_csv("simulations/dqa_baseline_context_eig.csv")


# 95% CIs
quantile(sim_context_df %>% select(EIG_kids) %>% unlist(), 
         c(0.025, 0.975))
quantile(sim_context_df %>% select(EIG_adults) %>% unlist(), 
         c(0.025, 0.975))

# full distribution
quantile(sim_context_df %>% select(EIG_kids) %>% unlist(), 
         c(0, 1))
quantile(sim_context_df %>% select(EIG_adults) %>% unlist(), 
         c(0, 1))

# actual mean EIGs in data
df_valid %>% group_by(AgeGroup) %>% 
  summarize(m_eig = mean(EIG))



## ----reuse and recombination across trials---------------------------------------------------------------------

# only valid trials (valid questions with at least one valid previous trial)
df_seq_consec <- df %>% filter(!is.na(same_as_last))
nrow(df_seq_consec)
table(df_seq_consec$AgeGroup)


##### reuse #####
df_seq_consec$same_as_last <- as.factor(df_seq_consec$same_as_last)

# how much repetition from earlier trials?
df_seq_consec %>% group_by(AgeGroup) %>%
  summarize(m = sum(same_as_last == 1)/n())

### within kids
reusetrials.kid <- glmer(same_as_last ~ age_mo_center + (1|id), 
              data = df_seq_consec %>% filter(AgeGroup == "Kids"), family = "binomial")
summary(reusetrials.kid)
anova(reusetrials.kid, update(reusetrials.kid, .~.-age_mo_center))
# no effect - just kids vs. adults for main analysis

reusetrials.all <- glmer(same_as_last ~ AgeGroup + (1|id), 
              data = df_seq_consec, family = "binomial")
summary(reusetrials.all)
anova(reusetrials.all, update(reusetrials.all, .~.-AgeGroup))
exp(Confint(reusetrials.all))

##### trial-to-trial recombination ####

## norm tree edit distance
df_seq_consec$dist_to_last_normed <- 1 - df_seq_consec$dist_to_last/max(df_seq_consec$dist_to_last, na.rm = T)



### tree dist 
remixtrials.kid <- lmer(dist_to_last_normed ~ age_mo_center + (1|id), 
                        data = df_seq_consec %>% filter(same_as_last == 0 & AgeGroup == "Kids"),
                        control = lmerControl(optimizer = "bobyqa"), REML = FALSE)
summary(remixtrials.kid)
anova(remixtrials.kid, update(remixtrials.kid, .~.-age_mo_center))
# significant -- split age group for main analysis

remixtrials.all <- lmer(dist_to_last_normed ~ AgeGroup_Split + (1|id), 
                        data = df_seq_consec %>% filter(same_as_last == 0))
summary(remixtrials.all)
anova(remixtrials.all, update(remixtrials.all, .~.-AgeGroup_Split))
Confint(remixtrials.all)


stargazer(remixtrials.all, type = "text",
          dep.var.labels = "Grammar-based similarity to most-similar previous question",
          ci = TRUE, digits = 2, digits.extra = 3, single.row = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001), omit.stat = c("aic", "bic", "ll", "n"))



### text similarity

# within kids
remixtrials.kid <- lmer(sim_to_last_standard ~ age_mo_center + (1|id), 
              data = df_seq_consec %>% filter(same_as_last == 0 & AgeGroup == "Kids"))
summary(remixtrials.kid)
anova(remixtrials.kid, update(remixtrials.kid, .~.-age_mo_center))
# no effect - just kids vs adults for main analysis

remixtrials.all <- lmer(sim_to_last_standard ~ AgeGroup + (1|id), 
             data = df_seq_consec %>% filter(same_as_last == 0))
summary(remixtrials.all)
anova(remixtrials.all, update(remixtrials.all, .~.-AgeGroup))
Confint(remixtrials.all)



#### NULL DISTRIBUTIONS #####

sim_reuse_df <- read_csv("simulations/dqa_baseline_reuse.csv")
sim_remixing_df <- read_csv("simulations/dqa_baseline_remixing.csv")

# norm tree edit distance to same scale as true data
sim_remixing_df$treedist_mean_normed <- 1 - sim_remixing_df$treedist_mean/max(df_seq_consec$dist_to_last, na.rm = T)


# we've simulated 1000 new datasets, where the trial structures match the 
# set of trials seen by one participant
# each questions and corresponding trial-to-trial statistics are sampled from all
# kids and adults who ever saw that trial


#### null for reuse #####
# true reuse rates
reuse_rates <- tapply(df_seq_consec$same_as_last, df_seq_consec$AgeGroup, function(x) {sum(x == 1)/length(x)})

# p-value: what proportion of our bootstrapped means are as or more extreme than the observed mean?
sum(sim_reuse_df %>% select(reuse_rate) >= reuse_rates["Kids"])/1000
sum(sim_reuse_df %>% select(reuse_rate) >= reuse_rates["Adults"])/1000

# this is the range of the distribution
sim_reuse_df %>% select(reuse_rate) %>% unlist() %>% range()
# true values are much higher
reuse_rates


#### null for recombination #####

df_seq_consec_nonmatch <- df_seq_consec %>% filter(same_as_last == 0)

## tree edit distance 
mean_dist <- tapply(df_seq_consec_nonmatch$dist_to_last_normed, df_seq_consec_nonmatch$AgeGroup_Split, mean)
mean_dist

## text similarity, standardized
mean_sim_standard <- tapply(df_seq_consec_nonmatch$sim_to_last_standard, df_seq_consec_nonmatch$AgeGroup, mean)
mean_sim_standard

# p-value: what proportion of our bootstrapped means are as or more extreme than the observed mean?
sum(sim_remixing_df %>% select(treedist_mean_normed) >= mean_dist["5- to 6-year-olds"])/1000
sum(sim_remixing_df %>% select(treedist_mean_normed) >= mean_dist["7- to 8-year-olds"])/1000
sum(sim_remixing_df %>% select(treedist_mean_normed) >= mean_dist["9- to 10-year-olds"])/1000

sum(sim_remixing_df %>% select(treedist_mean_normed) >= mean_dist["Adults"])/1000


# p-value: what proportion of our bootstrapped means are as or more extreme than the observed mean?
sum(sim_remixing_df %>% select(textsim_mean) >= mean_sim_standard["Kids"])/1000
sum(sim_remixing_df %>% select(textsim_mean) >= mean_sim_standard["Adults"])/1000


# this is the full range of the null
sim_remixing_df %>% select(treedist_mean_normed) %>% unlist() %>% range()
# true values are mostly lower
mean_dist

# this is the full range of the null
sim_remixing_df %>% select(textsim_mean) %>% unlist() %>% range() 
# true values are mostly higher
mean_sim_standard

