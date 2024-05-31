## ----load data---------------------------------------------------------------------------------------------

library(tidyverse)
library(lme4)
library(stargazer)
library(car)
library(broom.mixed)

setwd(this.path::here())

df2 <- read_csv("data/dqa_exposure_preppeddata.csv")

df2 <- df2 %>% mutate(
  AgeGroup = as.factor(AgeGroup),
  id = as.factor(id),
  trial_index = as.factor(trial_index),
  question_cond = as.factor(question_cond),
  quality_cond = as.factor(quality_cond),
  exposure_cond = as.factor(exposure_cond),
  trial_type = as.factor(trial_type),
  target_match = as.factor(target_match)
)

df2$AgeGroup <- relevel(df2$AgeGroup, ref = "Kids")


## ----study 2 participants----------------------------------------------------------------------------------


n_parts2 <- df2 %>%
  group_by(AgeGroup) %>%
  summarize(n = n() / 4)

ages2 <- df2 %>%
  filter(AgeGroup == "Kids") %>%
  group_by(age_yr) %>%
  summarize(n = n() / 4)

n_parts2
ages2

df2 %>% 
  group_by(exposure_cond) %>%
  summarize(n = n() / 4)

## ----skips-------------------------------------------------------------------------------------------------

n_skip_kids2 <- sum(df2[df2$AgeGroup == "Kids", "notes"] == "missing" | df2[df2$AgeGroup == "Kids", "notes"] == "bad audio", na.rm = T)
n_skip_adults2 <- sum(df2[df2$AgeGroup == "Adults", "notes"] == "missing" | df2[df2$AgeGroup == "Adults", "notes"] == "bad audio", na.rm = T)

total_trials_kids2 <- df2 %>%
  filter(AgeGroup == "Kids") %>%
  nrow()

total_trials_adults2 <- df2 %>%
  filter(AgeGroup == "Adults") %>%
  nrow()

#proportion skipped/bad audio:
(n_skip_kids2 + n_skip_adults2)/(total_trials_kids2 + total_trials_adults2)


## ----study 2 descriptives, valid and invalid questions-----------------------------------------------------

n_invalid_kids2 <- sum(df2[df2$AgeGroup == "Kids", "notes"] == "invalid", na.rm = T)
n_invalid_adults2 <- sum(df2[df2$AgeGroup == "Adults", "notes"] == "invalid", na.rm = T)
(n_invalid_kids2 + n_invalid_adults2)/(total_trials_kids2 + total_trials_adults2)

n_ambig_kids2 <- sum(df2[df2$AgeGroup == "Kids", "notes"] == "ambiguous", na.rm = T)
n_ambig_adults2 <- sum(df2[df2$AgeGroup == "Adults", "notes"] == "ambiguous", na.rm = T)
(n_ambig_kids2 + n_ambig_adults2)/(total_trials_kids2 + total_trials_adults2)

n_gram_kids2 <- sum(df2[df2$AgeGroup == "Kids", "notes"] == "well-defined but not in grammar", na.rm = T)
n_gram_adults2 <- sum(df2[df2$AgeGroup == "Adults", "notes"] == "well-defined but not in grammar", na.rm = T)
(n_gram_kids2 + n_gram_adults2)/(total_trials_kids2 + total_trials_adults2)


##### for supplement: descriptives about excluded trials ####

df2$trouble_response <- as.factor(ifelse(is.na(df2$question_program), "yes", "no"))

by_part_trials <- df2 %>% group_by(id, AgeGroup) %>%
  summarize(n_valid_trials = sum(trouble_response == "no"))

table(by_part_trials$AgeGroup, by_part_trials$n_valid_trials)

excl.mod.kid <- glmer(trouble_response ~ age_mo + (1|id) + (1|trial_index), 
              data = df2 %>% filter(AgeGroup == "Kids"),
              family = "binomial")
drop1(excl.mod.kid, test = "Chisq")

excl.mod.all <- glmer(trouble_response ~ AgeGroup_Split + (1|id) + (1|trial_index), 
              data = df2,
              family = "binomial")
summary(excl.mod.all)
Confint(excl.mod.all, exponentiate = TRUE)
drop1(excl.mod.all, test = "Chisq")

mod_results <- tidy(excl.mod.all, conf.int = T, exponentiate = TRUE)

stargazer(excl.mod.all, type = "text",
          dep.var.labels = "Excluded response",
          ci = TRUE, digits = 2, digits.extra = 3, single.row = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001), omit.stat = c("aic", "bic", "ll", "n"),
          coef = list(mod_results$estimate), 
          ci.custom = list(as.matrix(mod_results[,c("conf.low", "conf.high")])),
          p = list(mod_results$p.value), 
          covariate.labels = c("Age Group [7- to 8-year-olds]", 
                               "Age Group [9- to 10-year-olds]", "Age Group [Adults]", "Intercept"))




#just look at valid and coded questions from here on out
df_valid2 <- df2 %>% filter(!is.na(question_program))

summary_final_sample <- df_valid2 %>% group_by(AgeGroup) %>%
  summarize(n_part = length(unique(id)),
            n_q = n())
summary_final_sample



## ---- Do children and adults reuse questions? --------------------------------------------------------------------------------

#### just within kids
reuse.kid.int <- glmer(
  target_match ~ exposure_cond * age_mo_center +
    (1 | id) + (1|trial_type) + (1|question_cond),
  data = df_valid2 %>% filter(trial_type != "zero" & AgeGroup == "Kids"),
  family = "binomial", control = glmerControl(optimizer = "bobyqa"))
summary(reuse.kid.int)
drop1(reuse.kid.int, test = "Chisq")  # no interaction, proceed to main effects

reuse.kid <- glmer(
  target_match ~ exposure_cond + age_mo_center +
    (1 | id) + (1|trial_type) + (1|question_cond),
  data = df_valid2 %>% filter(trial_type != "zero" & AgeGroup == "Kids"),
  family = "binomial", control = glmerControl(optimizer = "bobyqa"))
summary(reuse.kid)
drop1(reuse.kid, test = "Chisq") # main effect of age (in months) -- use bins in model

# BUT the model with binned age group has problems because of zero cell counts
the_data <- df_valid2 %>% filter(trial_type != "zero")
table(the_data$AgeGroup_Split, the_data$target_match, the_data$exposure_cond)
# so instead we'll just have to compare children vs. adults

##### children vs. adults
reuse.all.int <- glmer(
  target_match ~ exposure_cond * AgeGroup +
    (1 | id) + (1|trial_type) + (1|question_cond),
  data = df_valid2 %>% filter(trial_type != "zero"),
  family = "binomial", control = glmerControl(optimizer = "bobyqa"))
summary(reuse.all.int)
drop1(reuse.all.int, test = "Chisq") # no interaction, proceed to main effects

reuse.all <- glmer(
  target_match ~ exposure_cond + AgeGroup +
    (1 | id) + (1|trial_type) + (1|question_cond),
  data = df_valid2 %>% filter(trial_type != "zero"),
  family = "binomial")
summary(reuse.all)
drop1(reuse.all, test = "Chisq") # effect of age group and exposure_cond
exp(Confint(reuse.all))

## ----reuse and prior informativeness--------------------------------------------------------------------------------

##### just within kids
reuse.prior.kid.int <- glmer(
  target_match ~ quality_cond * age_mo_center +
    (1 | id) + (1 | question_cond) + (1 | trial_type),
  data = df_valid2 %>% filter(trial_type != "zero" & exposure_cond == "exposure" & AgeGroup == "Kids"),
  family = "binomial", control = glmerControl(optimizer = "bobyqa")
)
summary(reuse.prior.kid.int)
drop1(reuse.prior.kid.int, test = "Chisq") # no interaction, proceed to main effects


reuse.prior.kid <- glmer(
  target_match ~ quality_cond + age_mo_center +
    (1 | id) + (1 | question_cond) + (1 | trial_type),
  data = df_valid2 %>% filter(trial_type != "zero" & exposure_cond == "exposure" & AgeGroup == "Kids"),
  family = "binomial", control = glmerControl(optimizer = "bobyqa")
)
summary(reuse.prior.kid)
drop1(reuse.prior.kid, test = "Chisq") # main effect of age, use age group bins

###### comparing children vs. adults
reuse.prior.all.int <- glmer(
  target_match ~ quality_cond * AgeGroup_Split +
    (1 | id) + (1 | question_cond) + (1 | trial_type),
  data = df_valid2 %>% filter(trial_type != "zero" & exposure_cond == "exposure"),
  family = "binomial", control = glmerControl(optimizer = "bobyqa")
)
summary(reuse.prior.all.int)
drop1(reuse.prior.all.int, test = "Chisq") #no interaction, proceed to main effects


reuse.prior.all <- glmer(
  target_match ~ quality_cond + AgeGroup_Split +
    (1 | id) + (1 | question_cond) + (1 | trial_type),
  data = df_valid2 %>% filter(trial_type != "zero" & exposure_cond == "exposure"),
  family = "binomial"
)
summary(reuse.prior.all)
drop1(reuse.prior.all, test = "Chisq") # main effect of age group, but not quality cond
exp(Confint(reuse.prior.all))

##### make full regression table for supplement
mod_results <- tidy(reuse.prior.all, conf.int = T, exponentiate = TRUE)

stargazer(reuse.prior.all, type = "text",
          dep.var.labels = "Match to target question",
          ci = TRUE, digits = 2, digits.extra = 3, single.row = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001), omit.stat = c("aic", "bic", "ll", "n"),
          coef = list(mod_results$estimate), 
          ci.custom = list(as.matrix(mod_results[,c("conf.low", "conf.high")])),
          p = list(mod_results$p.value), 
          covariate.labels = c("Previous Quality Condition [Previously-informative]", "Age Group [7- to 8-year-olds]", 
                               "Age Group [9- to 10-year-olds]", "Age Group [Adults]", "Intercept"))




## ----reuse and current informativeness--------------------------------------------------------------------------------

#### just within kids
reuse.current.kid.int <- glmer(
  target_match ~ trial_type * age_mo_center +
    (1 | id) + (1 | question_cond),
  data = df_valid2 %>% filter(exposure_cond == "exposure" & AgeGroup == "Kids"),
  family = "binomial", control = glmerControl(optimizer = "bobyqa")
)
summary(reuse.current.kid.int)
drop1(reuse.current.kid.int, test = "Chisq") ## no interaction, proceed to main effects

reuse.current.kid <- glmer(
  target_match ~ trial_type + age_mo_center +
    (1 | id) + (1 | question_cond),
  data = df_valid2 %>% filter(exposure_cond == "exposure" & AgeGroup == "Kids"),
  family = "binomial", control = glmerControl(optimizer = "bobyqa")
)
summary(reuse.current.kid)
drop1(reuse.current.kid, test = "Chisq") ## effect of age, use binned age group for main analyses

# but we again have problems with zero cell counts, so the model won't fit
table(df_valid2$AgeGroup_Split, df_valid2$target_match, df_valid2$trial_type)
# so we have to collapse across age bins and just compare children vs. adults

###### kids vs. adults
reuse.current.all.int <- glmer(
  target_match ~ trial_type * AgeGroup +
    (1 | id) + (1 | question_cond),
  data = df_valid2 %>% filter(exposure_cond == "exposure"),
  family = "binomial", control = glmerControl(optimizer="bobyqa"))
drop1(reuse.current.all.int, test = "Chisq") # no interaction, proceed to main effects


reuse.current.all <- glmer(
  target_match ~ trial_type + AgeGroup +
    (1 | id) + (1 | question_cond),
  data = df_valid2 %>% filter(exposure_cond == "exposure"),
  family = "binomial"
)
summary(reuse.current.all)
drop1(reuse.current.all, test = "Chisq") # main effect of both age group and trial type
exp(Confint(reuse.current.all))


## ---- Do children and adults remix questions? --------------------------------------------------------------------------------

df_nonmatch <- subset(df_valid2, df_valid2$target_match != 1)

#interaction within kids
remix.kid.int <- lmer(
  target_sim_standard ~ exposure_cond * age_mo_center +
    (1 | id) + (1 | question_cond) + (1 | trial_type),
  data = df_nonmatch %>% filter(AgeGroup == "Kids")
)
summary(remix.kid.int)
drop1(remix.kid.int, test = "Chisq") # no interaction

### main effects within kids
remix.kid <- lmer(
  target_sim_standard ~ exposure_cond + age_mo_center +
    (1 | id) + (1 | question_cond) + (1 | trial_type),
  data = df_nonmatch %>% filter(AgeGroup == "Kids")
)
summary(remix.kid)
drop1(remix.kid, test = "Chisq") # effect of age > .01 (preregistered cutoff)

### interaction kids vs. adults
remix.all.int <- lmer(
  target_sim_standard ~ exposure_cond * AgeGroup +
    (1 | id) + (1 | question_cond) + (1 | trial_type),
  data = df_nonmatch
)
summary(remix.all.int)
drop1(remix.all.int, test = "Chisq") #no interaction 

### main effects kids vs. adults
remix.all <- lmer(
  target_sim_standard ~ exposure_cond + AgeGroup +
    (1 | id) + (1 | question_cond) + (1 | trial_type),
  data = df_nonmatch, control = lmerControl(optimizer = "bobyqa")
)
summary(remix.all)
drop1(remix.all, test = "Chisq") # main effects of exposure_cond and Age Group
Confint(remix.all)

## ---- remixing and prior informativeness--------------------------------------------------------------------------------

remix.prior.kid.int <- lmer(
  target_sim_standard ~ quality_cond * age_mo_center +
    (1 | id) + (1 | question_cond) + (1 | trial_type),
  data = df_nonmatch %>% filter(exposure_cond == "exposure" & AgeGroup == "Kids")
)
summary(remix.prior.kid.int)
drop1(remix.prior.kid.int, test = "Chisq") # no interaction, proceed to main effects

remix.prior.kid <- lmer(
  target_sim_standard ~ quality_cond + age_mo_center +
    (1 | id) + (1 | question_cond) + (1 | trial_type),
  data = df_nonmatch %>% filter(exposure_cond == "exposure" & AgeGroup == "Kids")
)
summary(remix.prior.kid)
drop1(remix.prior.kid, test = "Chisq") # no effect of age or quality cond 


#### kids vs. adults
remix.prior.all.int <- lmer(
  target_sim_standard ~ quality_cond * AgeGroup +
    (1 | id) + (1 | question_cond) + (1 | trial_type),
  data = df_nonmatch %>% filter(exposure_cond == "exposure")
)
summary(remix.prior.all.int)
drop1(remix.prior.all.int, test = "Chisq") # no interaction


#### kids vs. adults, main effects
remix.prior.all <- lmer(
  target_sim_standard ~ quality_cond + AgeGroup +
    (1 | id) + (1 | question_cond) + (1 | trial_type),
  data = df_nonmatch %>% filter(exposure_cond == "exposure")
)
summary(remix.prior.all)
drop1(remix.prior.all, test = "Chisq") # effect of age but not quality cond
Confint(remix.prior.all)


## ---- EXPLORATORY: remixing and CURRENT informativeness--------------------------------------------------------------------------------

df_nonmatch %>% filter(exposure_cond == "exposure") %>%
  group_by(trial_type) %>%
  summarize(m = mean(target_sim_standard),
            sd = sd(target_sim_standard))

remix.current.all <- lmer(
  target_sim_standard ~ trial_type +
    (1 | id) + (1 | question_cond),
  data = df_nonmatch %>% filter(exposure_cond == "exposure"),
)
summary(remix.current.all)
drop1(remix.current.all, test = "Chisq") # main effect of trial type

# table for supplement
stargazer(remix.current.all, type = "text",
          dep.var.labels = "Similarity to target question",
          ci = TRUE, digits = 2, digits.extra = 3, single.row = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001), omit.stat = c("aic", "bic", "ll", "n"),
          covariate.labels = c("Trial type [medium]",
                               "Trial type [too-complex]",
                               "Trial type [zero]", 
                               "Intercept"), p.auto = FALSE)




remix.cond.all <- lmer(
  target_sim_standard ~ trial_type*exposure_cond +
    (1 | id) + (1 | question_cond),
  data = df_nonmatch,
)
summary(remix.cond.all)
drop1(remix.cond.all, test = "Chisq") # no interaction


## ----EXPLORATORY: Do reuse and remixing help people ask better questions?---------------------------------------------------------------------------------------------------


#### does this vary by age? ####
eig.kid.int <- lmer(EIG ~ exposure_cond*age_mo_center + 
               (1|id) + (1|trial_type) + (1|question_cond),
             data = df_valid2 %>% filter(AgeGroup == "Kids"))
summary(eig.kid.int)
drop1(eig.kid.int, test = "Chisq") # no interaction within kids

eig.kid <- lmer(EIG ~ exposure_cond + age_mo_center + 
               (1|id) + (1|trial_type) + (1|question_cond),
             data = df_valid2 %>% filter(AgeGroup == "Kids"))
summary(eig.kid)
drop1(eig.kid, test = "Chisq") # no effect of age within kids

eig.all.int <- lmer(EIG ~ exposure_cond*AgeGroup + 
               (1|id) + (1|trial_type) + (1|question_cond),
             data = df_valid2)
summary(eig.all.int)
drop1(eig.all.int, test = "Chisq") # no interaction 


eig.all <- lmer(EIG ~ exposure_cond + AgeGroup + 
               (1|id) + (1|trial_type) + (1|question_cond),
             data = df_valid2)
summary(eig.all)
drop1(eig.all, test = "Chisq") # main effects of both exposure_cond and age group
Confint(eig.all)

##### is this a result of reuse, remixing, or both? #####

eig.reuse <- lmer(EIG ~ target_match + 
               (1|id) + (1|trial_type) +(1|question_cond),
             data = df_valid2)
summary(eig.reuse)
drop1(eig.reuse, test = "Chisq") ## interaction with trial type
Confint(eig.reuse)


eig.reusecond <- lmer(EIG ~ target_match + exposure_cond + 
               (1|id) + (1|trial_type) +(1|question_cond),
             data = df_valid2)
summary(eig.reusecond)
drop1(eig.reusecond, test = "Chisq") ## interaction with trial type
Confint(eig.reusecond)



## ----EXPLORATORY: Trial-to-trial reuse/remixing---------------------------------------------------------------------------------------------------


df_seq_consec <- df_valid2 %>% filter(!is.na(same_as_last))

nrow(df_seq_consec)
table(df_seq_consec$AgeGroup)


###### Does trial-to-trial reuse differ by age? #####

df_seq_consec$same_as_last <- as.factor(df_seq_consec$same_as_last)

reusetrials.kid <- glmer(same_as_last ~ age_mo_center + (1|id), 
              data = df_seq_consec %>% filter(AgeGroup == "Kids"), family = "binomial")
summary(reusetrials.kid)
drop1(reusetrials.kid, test = "Chisq") # effect of age, so bin age group


reusetrials.all <- glmer(same_as_last ~ AgeGroup_Split + (1|id), 
              data = df_seq_consec, family = "binomial")
summary(reusetrials.all)
drop1(reusetrials.all, test = "Chisq") 
Confint(reusetrials.all)

##### make full regression table for supplement
mod_results <- tidy(reusetrials.all, conf.int = T, exponentiate = TRUE)
mod_results <- mod_results %>% filter(effect == "fixed")

stargazer(reusetrials.all, type = "text",
          dep.var.labels = "Match to any previously-asked question",
          ci = TRUE, digits = 2, digits.extra = 3, single.row = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001), omit.stat = c("aic", "bic", "ll", "n"),
          coef = list(mod_results$estimate), 
          ci.custom = list(as.matrix(mod_results[,c("conf.low", "conf.high")])),
          p = list(mod_results$p.value), 
          covariate.labels = c("Age Group [7- to 8-year-olds]", 
                               "Age Group [9- to 10-year-olds]", "Age Group [Adults]", "Intercept"))




###### Does trial-to-trial remixing differ by age? #####
df_seq_consec_nonmatch <- df_seq_consec %>% filter(same_as_last == 0)


### sim_to_last (text similarity)
remixingtrials.kid <- lmer(sim_to_last_standard ~ age_mo_center + (1|id), 
             data = df_seq_consec_nonmatch %>% filter(AgeGroup == "Kids"),
             control = lmerControl(optimizer = "bobyqa"))
summary(remixingtrials.kid)
drop1(remixingtrials.kid, test = "Chisq") ## no effect of age (in kids)

remixingtrials.all <- lmer(sim_to_last_standard ~ AgeGroup + (1|id), 
             data = df_seq_consec_nonmatch)
summary(remixingtrials.all)
drop1(remixingtrials.all, test = "Chisq") ## no effects
Confint(remixingtrials.all)


##### trial-to-trial reuse/remixing, comparison to null ########

sim_reuse_df <- read_csv("simulations/dqa_exposure_reuse.csv")
sim_remixing_df <- read_csv("simulations/dqa_exposure_remixing.csv")



# we've simulated 1000 new datasets, where the paired trials match the paired 
# set of trials seen by one participant
# each question and corresponding trial-to-trial statistics are sampled from ALL
# kids and adults who ever saw that question


reuse_rates <- tapply(df_seq_consec$same_as_last, df_seq_consec$AgeGroup, function(x) {sum(x == 1)/length(x)})

#### null for reuse #####

# p-value: what proportion of our bootstrapped means are as or more extreme than the observed mean?
sum(sim_reuse_df %>% select(reuse_rate) >= reuse_rates["Kids"])/1000
sum(sim_reuse_df %>% select(reuse_rate) >= reuse_rates["Adults"])/1000


# this is the full range
quantile(sim_reuse_df %>% select(reuse_rate) %>% unlist(), 
         c(0, 1))
# true values are much higher
reuse_rates


#### null for remixing #####

df_seq_consec_nonmatch <- df_seq_consec %>% filter(same_as_last == 0)

## text similarity, standardized
mean_sim_standard <- tapply(df_seq_consec_nonmatch$sim_to_last_standard, df_seq_consec_nonmatch$AgeGroup, mean)
mean_sim_standard


# p-value: what proportion of our bootstrapped means are as or more extreme than the observed mean?
sum(sim_remixing_df %>% select(remixing_mean) >= mean_sim_standard["Kids"])/1000
sum(sim_remixing_df %>% select(remixing_mean) >= mean_sim_standard["Adults"])/1000


# this is the full range of the null
quantile(sim_remixing_df %>% select(remixing_mean) %>% unlist(), 
         c(0, 1))
# true values are higher
mean_sim_standard


######################### SUPPLEMENT: PREREGISTERED REMIXING ANALYSES ###########################


## ---- Do children and adults remix questions? --------------------------------------------------------------------------------


#interaction within kids
remix.dist.kid.int <- lmer(
  target_dist ~ exposure_cond * age_mo_center +
    (1 | id) + (1 | question_cond) + (1 | trial_type),
  data = df_nonmatch %>% filter(AgeGroup == "Kids")
)
summary(remix.dist.kid.int)
drop1(remix.dist.kid.int, test = "Chisq") # no interaction

### main effects within kids
remix.dist.kid <- lmer(
  target_dist ~ exposure_cond + age_mo_center +
    (1 | id) + (1 | question_cond) + (1 | trial_type),
  data = df_nonmatch %>% filter(AgeGroup == "Kids")
)
summary(remix.dist.kid)
drop1(remix.dist.kid, test = "Chisq") # effect of age > .01

### interaction kids vs. adults
remix.dist.all.int <- lmer(
  target_dist ~ exposure_cond * AgeGroup +
    (1 | id) + (1 | question_cond) + (1 | trial_type),
  data = df_nonmatch
)
summary(remix.dist.all.int)
drop1(remix.dist.all.int, test = "Chisq") #no interaction 

### main effects kids vs. adults
remix.dist.all <- lmer(
  target_dist ~ exposure_cond + AgeGroup +
    (1 | id) + (1 | question_cond) + (1 | trial_type),
  data = df_nonmatch, control = lmerControl(optimizer = "bobyqa")
)
summary(remix.dist.all)
drop1(remix.dist.all, test = "Chisq") #main effect of age group, exposure cond > .01
Confint(remix.dist.all)

## ---- remixing and prior informativeness--------------------------------------------------------------------------------

##### interactions

remix.dist.prior.kid.int <- lmer(
  target_dist ~ quality_cond * age_mo_center +
    (1 | id) + (1 | question_cond) + (1 | trial_type),
  data = df_nonmatch %>% filter(exposure_cond == "exposure" & AgeGroup == "Kids")
)
summary(remix.dist.prior.kid.int)
drop1(remix.dist.prior.kid.int, test = "Chisq")

remix.dist.prior.kid <- lmer(
  target_dist ~ quality_cond + age_mo_center +
    (1 | id) + (1 | question_cond) + (1 | trial_type),
  data = df_nonmatch %>% filter(exposure_cond == "exposure" & AgeGroup == "Kids")
)
summary(remix.dist.prior.kid)
drop1(remix.dist.prior.kid, test = "Chisq")


#### kids vs. adults
remix.dist.prior.all.int <- lmer(
  target_dist ~ quality_cond * AgeGroup +
    (1 | id) + (1 | question_cond) + (1 | trial_type),
  data = df_nonmatch %>% filter(exposure_cond == "exposure")
)
summary(remix.dist.prior.all.int)
drop1(remix.dist.prior.all.int, test = "Chisq")



#### kids vs. adults, main effects
remix.dist.prior.all <- lmer(
  target_dist ~ quality_cond + AgeGroup +
    (1 | id) + (1 | question_cond) + (1 | trial_type),
  data = df_nonmatch %>% filter(exposure_cond == "exposure")
)
summary(remix.dist.prior.all)
drop1(remix.dist.prior.all, test = "Chisq")
Confint(remix.dist.prior.all)

