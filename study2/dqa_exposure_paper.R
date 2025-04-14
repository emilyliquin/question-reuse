## ----load data---------------------------------------------------------------------------------------------

library(tidyverse)
library(lme4)
library(stargazer)
library(car)
library(broom.mixed)
library(emmeans)
library(lmerTest)

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
anova(excl.mod.kid, update(excl.mod.kid, . ~ . - age_mo)) # significant age effect in kids

excl.mod.all <- glmer(trouble_response ~ AgeGroup_Split + (1|id) + (1|trial_index), 
                      data = df2,
                      family = "binomial")
summary(excl.mod.all)
Confint(excl.mod.all, exponentiate = TRUE)
anova(excl.mod.all, update(excl.mod.all, . ~ . - AgeGroup_Split))

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


## norm tree edit distance
df_valid2$target_dist_normed <- 1-df_valid2$target_dist/max(df_valid2$target_dist, na.rm = T)
df_valid2$dist_to_last_normed <- 1-df_valid2$dist_to_last/max(df_valid2$dist_to_last, na.rm = T)

##### for supplement: answering the target question correctly #####

df_valid2$answer_correct <- ifelse(df_valid2$exp_answer_response == df_valid2$exp_answer_correct, 1, 0)

answerdf <- df_valid2 %>% group_by(id, exposure_cond, question_cond, AgeGroup, AgeGroup_Split, age_mo_center) %>%
  summarize(answer_correct = answer_correct[1])

practiceans.kid.int <- glm(answer_correct ~ age_mo_center*question_cond, 
          data = answerdf, family = "binomial")
summary(practiceans.kid.int)
drop1(practiceans.kid.int, test = "Chisq")# no interaction

practiceans.kid <- glm(answer_correct ~ age_mo_center + question_cond, 
          data = answerdf, family = "binomial")
summary(practiceans.kid) # effect of age in months
drop1(practiceans.kid, test = "Chisq")
exp(Confint(practiceans.kid))

# proportion of oldest children who answer correctly
answerdf910 <- answerdf %>% filter(AgeGroup_Split == "9- to 10-year-olds")
table(answerdf910$answer_correct) #100%

# proportion of middle children who answer correctly
answerdf78 <- answerdf %>% filter(AgeGroup_Split == "7- to 8-year-olds")
table(answerdf78$answer_correct) #94%

# proportion of youngest children who answer correctly
answerdf56 <- answerdf %>% filter(AgeGroup_Split == "5- to 6-year-olds")
table(answerdf56$answer_correct) #82%

#is this above chance? 25% - four options
table(answerdf56$answer_correct)
chisq.test(table(answerdf56$answer_correct), p = c(0.75, 0.25))


# children vs. adults
practiceans.all.int <- glm(answer_correct ~ AgeGroup*question_cond, 
          data = answerdf, family = "binomial")
summary(practiceans.all.int) 
drop1(practiceans.all.int, test = "Chisq")# no interaction

practiceans.all <- glm(answer_correct ~ AgeGroup + question_cond, 
          data = answerdf, family = "binomial")
summary(practiceans.all)
drop1(practiceans.all, test = "Chisq")# no effects
exp(Confint(practiceans.all))

# proportion of adults who answer correctly
answerdfadult <- answerdf %>% filter(AgeGroup_Split == "Adults")
table(answerdfadult$answer_correct) #95%


####### Reuse -- is there an effect of exposure? ########

# contrast coding for all dichotomous variables
df_valid2$exposure_cond_code <- ifelse(df_valid2$exposure_cond == "baseline", -0.5, 0.5)
df_valid2$quality_cond_code <- ifelse(df_valid2$quality_cond == "bad", -0.5, 0.5)
df_valid2$question_cond_code <- ifelse(df_valid2$question_cond == "heads", -0.5, 0.5)
df_valid2$age_group_code <- ifelse(df_valid2$AgeGroup == "Kids", -0.5, 0.5)


### just focus on three trial types for this analysis (not zero) ###
df_valid2_sub <- df_valid2 %>% filter(trial_type %in% c("best", "nonbest", "overcomplex"))

# contrast coding for trial type
c2<-contr.treatment(3)
my.coding2<-matrix(rep(1/3, 6), ncol=2)
my.simple2<-c2-my.coding2
my.simple2

df_valid2_sub$trial_type_code <- as.factor(as.character(df_valid2_sub$trial_type))
contrasts(df_valid2_sub$trial_type_code) = my.simple2
# under this coding scheme:
# - intercept is grand mean
# - trial_type_code2 is difference between best and nonbest
# - trial_type_code3 is difference between best and overcomplex



#### just in kids
reuse.kids.int <- glmer(
  target_match ~ 
    exposure_cond_code * age_mo_center +
    quality_cond_code * age_mo_center +
    trial_type_code * age_mo_center +
    question_cond_code * age_mo_center +
    (1 | id),
  data = df_valid2_sub %>% filter(AgeGroup == "Kids" ),
  family = "binomial", control = glmerControl(optimizer = "bobyqa",
                                              optCtrl=list(maxfun=100000)))
summary(reuse.kids.int)
anova(reuse.kids.int, 
      update(reuse.kids.int, . ~ . - exposure_cond_code:age_mo_center)) # no interaction

reuse.kids <- glmer(
  target_match ~ 
    exposure_cond_code + 
    quality_cond_code + age_mo_center +
    trial_type_code + age_mo_center +
    question_cond_code + age_mo_center +
    (1 | id),
  data = df_valid2_sub %>% filter(AgeGroup == "Kids" ),
  family = "binomial", control = glmerControl(optimizer = "bobyqa",
                                              optCtrl=list(maxfun=100000)))
summary(reuse.kids) # no sig. effect of age at p < .01 level (p = .02)
# just do kids vs. adults for main analysis


reuse.all.int <- glmer(target_match ~ 
                 exposure_cond_code * age_group_code +
                 quality_cond_code * age_group_code + 
                 trial_type_code * age_group_code + 
                 question_cond_code * age_group_code +
                 (1 | id),
               data = df_valid2_sub,
               family = "binomial", control = glmerControl(optimizer = "bobyqa"))
summary(reuse.all.int)
anova(reuse.all.int, 
      update(reuse.all.int, . ~ . - exposure_cond_code:age_group_code)) # no interaction


reuse.all <- glmer(target_match ~ 
                 exposure_cond_code +
                 quality_cond_code * age_group_code + 
                 trial_type_code * age_group_code + 
                 question_cond_code * age_group_code +
                 (1 | id),
               data = df_valid2_sub,
               family = "binomial", control = glmerControl(optimizer = "bobyqa"))
summary(reuse.all)
anova(reuse.all, 
      update(reuse.all, . ~ . - exposure_cond_code)) #effect of exposure
exp(Confint(reuse.all))


(emm <- emmeans(reuse.all, revpairwise ~ age_group_code, type = "response"))
confint(emm)


##### make full regression table for supplement
mod_results <- tidy(reuse.all, conf.int = T, exponentiate = TRUE)
mod_results <- mod_results %>% filter(effect == "fixed")


stargazer(reuse.all, type = "text",
          dep.var.labels = "Match to target question",
          ci = TRUE, digits = 2, digits.extra = 3, single.row = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001), omit.stat = c("aic", "bic", "ll", "n"),
          coef = list(mod_results$estimate), 
          ci.custom = list(as.matrix(mod_results[,c("conf.low", "conf.high")])),
          p = list(mod_results$p.value), 
          covariate.labels = c("Exposure Condition [Exposure]", 
                               "Previous Quality Condition [Previously Informative]", 
                               "Age Group [Adults]", 
                               "Trial Type [Medium]",
                               "Trial Type [Too-Complex]",
                               "Question Condition [Legs]",
                               "Previous Quality Condition [Previously Informative] : Age Group [Adults]",
                               "Age Group [Adults] : Trial Type [Medium]",
                               "Age Group [Adults] : Trial Type [Too-Complex]",
                               "Age Group [Adults] : Question Condition [Legs]",
                               "Intercept"))



######## Reuse -- is there an effect of previous and current informativeness, in exposure condition? ###########

# re do trial type coding for all four levels
c2<-contr.treatment(4)
my.coding2<-matrix(rep(1/4, 12), ncol=3)
my.simple2<-c2-my.coding2
my.simple2

df_valid2$trial_type_code <- as.factor(as.character(df_valid2$trial_type))
contrasts(df_valid2$trial_type_code) = my.simple2
# under this coding scheme:
# - intercept is grand mean
# - trial_type_code2 is difference between best and nonbest
# - trial_type_code3 is difference between best and overcomplex
# - trial_type_code4 is difference between best and zero



#### just in kids
context.kid.int <- glmer(
  target_match ~ 
    quality_cond_code * age_mo_center +
    trial_type_code * age_mo_center +
    question_cond_code * age_mo_center +
    (1 | id),
  data = df_valid2 %>% filter(exposure_cond == "exposure" & AgeGroup == "Kids"),
  family = "binomial", control = glmerControl(optimizer = "bobyqa"))
summary(context.kid.int)
anova(context.kid.int, update(context.kid.int, .~. - quality_cond_code:age_mo_center))
anova(context.kid.int, update(context.kid.int, .~. - trial_type_code:age_mo_center))
 # no interactions

context.kid <- glmer(
  target_match ~ quality_cond_code +
    trial_type_code +
    question_cond_code * age_mo_center +
    (1 | id),
  data = df_valid2 %>% filter(exposure_cond == "exposure" & AgeGroup == "Kids"),
  family = "binomial", control = glmerControl(optimizer = "bobyqa"))
summary(context.kid) # effect of age not significant at preregistered p < .01 level
# so just do kids vs. adults for main analysis

### kids vs adults
context.all.int <- glmer(target_match ~ 
                 quality_cond_code * age_group_code + 
                 trial_type_code * age_group_code + 
                 question_cond_code * age_group_code +
                 (1 | id),
               data = df_valid2 %>% filter(exposure_cond == "exposure"),
               family = "binomial", control = glmerControl(optimizer = "bobyqa"))
summary(context.all.int)
anova(context.all.int, update(context.all.int, .~. - quality_cond_code:age_group_code))
anova(context.all.int, update(context.all.int, .~. - trial_type_code:age_group_code))


context.all <- glmer(target_match ~ 
                 quality_cond_code +
                 trial_type_code +
                 question_cond_code * age_group_code +
                 (1 | id),
               data = df_valid2 %>% filter(exposure_cond == "exposure"),
               family = "binomial", control = glmerControl(optimizer = "bobyqa"))
summary(context.all)
anova(context.all, update(context.all, .~. - quality_cond_code))
anova(context.all, update(context.all, .~. - trial_type_code))
exp(Confint(context.all))

##### make full regression table for supplement
mod_results <- tidy(context.all, conf.int = T, exponentiate = TRUE)
mod_results <- mod_results %>% filter(effect == "fixed")


stargazer(context.all, type = "text",
          dep.var.labels = "Match to target question",
          ci = TRUE, digits = 2, digits.extra = 3, single.row = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001), omit.stat = c("aic", "bic", "ll", "n"),
          coef = list(mod_results$estimate), 
          ci.custom = list(as.matrix(mod_results[,c("conf.low", "conf.high")])),
          p = list(mod_results$p.value), 
          covariate.labels = c("Previous Quality Condition [Previously Informative]", 
                               "Trial Type [Medium]",
                               "Trial Type [Too-Complex]",
                               "Trial Type [Worst]",
                               "Question Condition [Legs]",
                               "Age Group [Adults]", 
                               "Question Condition [Legs] : Age Group [Adults]",
                               "Intercept"))



######## Recombination -- is there an effect of exposure? ############

# just non-target-matching questions
df_nonmatch <- subset(df_valid2, df_valid2$target_match != 1)

### tree edit distance first

#### just in kids
recomb.kid.int1 <- lmer(
  target_dist_normed ~ 
    exposure_cond_code * age_mo_center +
    quality_cond_code * age_mo_center +
    trial_type_code * age_mo_center +
    question_cond_code * age_mo_center +
    (1 | id),
  data = df_nonmatch %>% filter(AgeGroup == "Kids" ))
summary(recomb.kid.int1)
anova(recomb.kid.int1, update(recomb.kid.int1, . ~ . - exposure_cond_code:age_mo_center)) # no interaction

recomb.kid1 <- lmer(
  target_dist_normed ~ 
    exposure_cond_code  +
    quality_cond_code * age_mo_center +
    trial_type_code * age_mo_center +
    question_cond_code * age_mo_center +
    (1 | id),
  data = df_nonmatch %>% filter(AgeGroup == "Kids" ))
summary(recomb.kid1) # no significant effect of age
# so just kids vs. adults for main analysis


recomb.all.int1 <- lmer(target_dist_normed ~ 
                exposure_cond_code * age_group_code +
                quality_cond_code * age_group_code + 
                trial_type_code * age_group_code + 
                question_cond_code * age_group_code +
                (1 | id),
              data = df_nonmatch)
summary(recomb.all.int1)
anova(recomb.all.int1, update(recomb.all.int1, . ~ . - exposure_cond_code:age_group_code))


recomb.all1 <- lmer(target_dist_normed ~ 
                exposure_cond_code +
                quality_cond_code * age_group_code + 
                trial_type_code * age_group_code + 
                question_cond_code * age_group_code +
                (1 | id),
              data = df_nonmatch)
summary(recomb.all1)
anova(recomb.all1, update(recomb.all1, . ~ . - exposure_cond_code))
Confint(recomb.all1)

(emm <- emmeans(recomb.all1, revpairwise ~ age_group_code))
confint(emm)

##### make full regression table for supplement
recomb.all1 <- lme4::lmer(target_dist_normed ~ 
             exposure_cond_code +
             quality_cond_code * age_group_code + 
             trial_type_code * age_group_code + 
             question_cond_code * age_group_code +
             (1 | id),
           data = df_nonmatch)


stargazer(recomb.all1, type = "text",
          dep.var.labels = "Grammar-based similarity to target question",
          ci = TRUE, digits = 2, digits.extra = 3, single.row = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001), omit.stat = c("aic", "bic", "ll", "n"),
          covariate.labels = c("Exposure Condition [Exposure]",
                               "Previous Quality Condition [Previously Informative]", 
                               "Age Group [Adults]", 
                               "Trial Type [Medium]",
                               "Trial Type [Too-Complex]",
                               "Trial Type [Worst]",
                               "Question Condition [Legs]",
                               "Previous Quality Condition [Previously Informative] : Age Group [Adults]", 
                               "Age Group [Adults] : Trial Type [Medium]",
                               "Age Group [Adults] : Trial Type [Too-Complex]",
                               "Age Group [Adults] : Trial Type [Worst]",
                               "Age Group [Adults] : Question Condition [Legs]",
                               "Intercept"))




##### now text sim 


#### just in kids
recomb.kid.int2 <- lmer(
  target_sim_standard ~ 
    exposure_cond_code * age_mo_center +
    quality_cond_code * age_mo_center +
    trial_type_code * age_mo_center +
    question_cond_code * age_mo_center +
    (1 | id),
  data = df_nonmatch %>% filter(AgeGroup == "Kids" ))
summary(recomb.kid.int2)
anova(recomb.kid.int2, update(recomb.kid.int2, . ~ . - exposure_cond_code: age_mo_center)) # no interaction


recomb.kid2 <- lmer(
  target_sim_standard ~ 
    exposure_cond_code +
    quality_cond_code * age_mo_center +
    trial_type_code * age_mo_center +
    question_cond_code * age_mo_center +
    (1 | id),
  data = df_nonmatch %>% filter(AgeGroup == "Kids" ))
summary(recomb.kid2) # age not sig. at p<.01 level
# so just kids vs. adults for next analysis



recomb.all.int2 <- lmer(target_sim_standard ~ 
                exposure_cond_code * age_group_code +
                quality_cond_code * age_group_code + 
                trial_type_code * age_group_code + 
                question_cond_code * age_group_code +
                (1 | id),
              data = df_nonmatch)
summary(recomb.all.int2)
anova(recomb.all.int2, update(recomb.all.int2, . ~ . - exposure_cond_code:age_group_code))


recomb.all2 <- lmer(target_sim_standard ~ 
                exposure_cond_code +
                quality_cond_code * age_group_code + 
                trial_type_code * age_group_code + 
                question_cond_code * age_group_code +
                (1 | id),
              data = df_nonmatch)
summary(recomb.all2)
anova(recomb.all2, update(recomb.all2, . ~ . - exposure_cond_code))
Confint(recomb.all2) 


(emm <- emmeans(recomb.all2, revpairwise ~ age_group_code))
confint(emm)



##### make full regression table for supplement
recomb.all2 <- lme4::lmer(target_sim_standard ~ 
                   exposure_cond_code +
                   quality_cond_code * age_group_code + 
                   trial_type_code * age_group_code + 
                   question_cond_code * age_group_code +
                   (1 | id),
                 data = df_nonmatch)


stargazer(recomb.all2, type = "text",
          dep.var.labels = "Text-based similarity to target question",
          ci = TRUE, digits = 2, digits.extra = 3, single.row = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001), omit.stat = c("aic", "bic", "ll", "n"),
          covariate.labels = c("Exposure Condition [Exposure]",
                               "Previous Quality Condition [Previously Informative]", 
                               "Age Group [Adults]", 
                               "Trial Type [Medium]",
                               "Trial Type [Too-Complex]",
                               "Trial Type [Worst]",
                               "Question Condition [Legs]",
                               "Previous Quality Condition [Previously Informative] : Age Group [Adults]", 
                               "Age Group [Adults] : Trial Type [Medium]",
                               "Age Group [Adults] : Trial Type [Too-Complex]",
                               "Age Group [Adults] : Trial Type [Worst]",
                               "Age Group [Adults] : Question Condition [Legs]",
                               "Intercept"))




######### Recombination -  is there an effect of previous informativeness, in exposure condition? ####################

### tree edit distance first

#### just in kids
recomb.context.kid.int1 <- lmer(
  target_dist_normed ~ 
    quality_cond_code * age_mo_center +
    trial_type_code * age_mo_center +
    question_cond_code * age_mo_center +
    (1 | id),
  data = df_nonmatch %>% filter(AgeGroup == "Kids" & exposure_cond == "exposure"))
summary(recomb.context.kid.int1)
anova(recomb.context.kid.int1, 
      update(recomb.context.kid.int1, . ~ . - quality_cond_code: age_mo_center))
#no significant interaction

recomb.context.kid1 <- lmer(
  target_dist_normed ~ 
    quality_cond_code +
    trial_type_code * age_mo_center +
    question_cond_code * age_mo_center +
    (1 | id),
  data = df_nonmatch %>% filter(AgeGroup == "Kids" & exposure_cond == "exposure"))
summary(recomb.context.kid1) # no sig. effect of age
# so just age group kids vs. adults in main analysis

### kids vs. adults
recomb.context.all.int1 <- lmer(target_dist_normed ~ 
                quality_cond_code * age_group_code + 
                trial_type_code * age_group_code + 
                question_cond_code * age_group_code +
                (1 | id),
              data = df_nonmatch %>% filter(exposure_cond == "exposure"))
summary(recomb.context.all.int1)
anova(recomb.context.all.int1, 
      update(recomb.context.all.int1, . ~ . - quality_cond_code: age_group_code))


recomb.context.all1 <- lmer(target_dist_normed ~ 
                quality_cond_code + 
                trial_type_code * age_group_code + 
                question_cond_code * age_group_code +
                (1 | id),
              data = df_nonmatch %>% filter(exposure_cond == "exposure"))
summary(recomb.context.all1)
anova(recomb.context.all1, 
      update(recomb.context.all1, . ~ . - quality_cond_code))
Confint(recomb.context.all1)
 


##### make full regression table for supplement
# this is also used to show the effect of trial type (exploratory)
recomb.context.all1 <- lme4::lmer(target_dist_normed ~ 
                   quality_cond_code + 
                   trial_type_code * age_group_code + 
                   question_cond_code * age_group_code +
                   (1 | id),
                 data = df_nonmatch %>% filter(exposure_cond == "exposure"))


stargazer(recomb.context.all1, type = "text",
          dep.var.labels = "Grammar-based similarity to target question",
          ci = TRUE, digits = 2, digits.extra = 3, single.row = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001), omit.stat = c("aic", "bic", "ll", "n"),
          covariate.labels = c("Previous Quality Condition [Previously Informative]",
                               "Trial Type [Medium]",
                               "Trial Type [Too-Complex]",
                               "Trial Type [Worst]",
                               "Age Group [Adults]", 
                               "Question Condition [Legs]",
                               "Trial Type [Medium]: Age Group [Adults]",
                               "Trial Type [Too-Complex] : Age Group [Adults]",
                               "Trial Type [Worst] : Age Group [Adults]",
                               "Age Group [Adults] : Question Condition [Legs]",
                               "Intercept"))


##### now text sim 

#### just in kids
recomb.context.kid.int2 <- lmer(
  target_sim_standard ~ 
    quality_cond_code * age_mo_center +
    trial_type_code * age_mo_center +
    question_cond_code * age_mo_center +
    (1 | id),
  data = df_nonmatch %>% filter(AgeGroup == "Kids" &exposure_cond == "exposure" ))
summary(recomb.context.kid.int2)
anova(recomb.context.kid.int2, 
      update(recomb.context.kid.int2, . ~ . - quality_cond_code: age_mo_center))
#no significant interaction

recomb.context.kid2 <- lmer(
  target_sim_standard ~ 
    quality_cond_code +
    trial_type_code * age_mo_center +
    question_cond_code * age_mo_center +
    (1 | id),
  data = df_nonmatch %>% filter(AgeGroup == "Kids" & exposure_cond == "exposure" ))
summary(recomb.context.kid2) # no effect of age - so just kids vs. adults in main analysis


# main analysis
recomb.context.all.int2 <- lmer(target_sim_standard ~ 
                quality_cond_code * age_group_code + 
                trial_type_code * age_group_code + 
                question_cond_code * age_group_code +
                (1 | id),
              data = df_nonmatch %>% filter(exposure_cond == "exposure"))
summary(recomb.context.all.int2)
anova(recomb.context.all.int2, 
      update(recomb.context.all.int2, . ~ . - quality_cond_code: age_group_code)) #not sig


recomb.context.all2 <- lmer(target_sim_standard ~ 
                quality_cond_code + 
                trial_type_code * age_group_code + 
                question_cond_code * age_group_code +
                (1 | id),
              data = df_nonmatch %>% filter(exposure_cond == "exposure"))
summary(recomb.context.all2)
anova(recomb.context.all2, update(recomb.context.all2, . ~ . - quality_cond_code))
Confint(recomb.context.all2)


##### make full regression table for supplement
# this is also used to show the effect of trial type (exploratory)
recomb.context.all2 <- lme4::lmer(target_sim_standard ~ 
                   quality_cond_code + 
                   trial_type_code * age_group_code + 
                   question_cond_code * age_group_code +
                   (1 | id),
                 data = df_nonmatch %>% filter(exposure_cond == "exposure"))


stargazer(recomb.context.all2, type = "text",
          dep.var.labels = "Text-based similarity to target question",
          ci = TRUE, digits = 2, digits.extra = 3, single.row = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001), omit.stat = c("aic", "bic", "ll", "n"),
          covariate.labels = c("Previous Quality Condition [Previously Informative]",
                               "Trial Type [Medium]",
                               "Trial Type [Too-Complex]",
                               "Trial Type [Worst]",
                               "Age Group [Adults]", 
                               "Question Condition [Legs]",
                               "Trial Type [Medium]: Age Group [Adults]",
                               "Trial Type [Too-Complex] : Age Group [Adults]",
                               "Trial Type [Worst] : Age Group [Adults]",
                               "Age Group [Adults] : Question Condition [Legs]",
                               "Intercept"))




## ----EXPLORATORY: Do reuse and recombination help people ask better questions?---------------------------------------------------------------------------------------------------


#### just in kids
eig.kid.int <- lmer(
  EIG ~ 
    exposure_cond_code * age_mo_center +
    quality_cond_code * age_mo_center +
    trial_type_code * age_mo_center +
    question_cond_code * age_mo_center +
    (1 | id),
  data = df_valid2 %>% filter(AgeGroup == "Kids" ))
summary(eig.kid.int)
anova(eig.kid.int, update(eig.kid.int, . ~ . - exposure_cond_code:age_mo_center))
#no interactions

eig.kid <- lmer(
  EIG ~ 
    exposure_cond_code  +
    quality_cond_code * age_mo_center +
    trial_type_code * age_mo_center +
    question_cond_code * age_mo_center +
    (1 | id),
  data = df_valid2 %>% filter(AgeGroup == "Kids" ))
summary(eig.kid) # no effect of age - just kids vs. adults for main analysis

# main analysis
eig.all.int <- lmer(EIG ~ 
                 exposure_cond_code * age_group_code +
                 quality_cond_code * age_group_code + 
                 trial_type_code * age_group_code + 
                 question_cond_code * age_group_code +
                 (1 | id),
               data = df_valid2)
summary(eig.all.int)
anova(eig.all.int, update(eig.all.int, . ~ . - exposure_cond_code:age_group_code))

eig.all <- lmer(EIG ~ 
                 exposure_cond_code +
                 quality_cond_code * age_group_code + 
                 trial_type_code * age_group_code + 
                 question_cond_code * age_group_code +
                 (1 | id),
               data = df_valid2)
summary(eig.all)
anova(eig.all, update(eig.all, . ~ . - exposure_cond_code))
Confint(eig.all)


(emm <- emmeans(eig.all, revpairwise ~ age_group_code, type = "response"))
confint(emm)

##### make full regression table for supplement
eig.all <- lme4::lmer(EIG ~ 
                   exposure_cond_code +
                   quality_cond_code * age_group_code + 
                   trial_type_code * age_group_code + 
                   question_cond_code * age_group_code +
                   (1 | id),
                 data = df_valid2)


stargazer(eig.all, type = "text",
          dep.var.labels = "EIG",
          ci = TRUE, digits = 2, digits.extra = 3, single.row = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001), omit.stat = c("aic", "bic", "ll", "n"),
          covariate.labels = c("Exposure Condition [Exposure]", 
                               "Previous Quality Condition [Previously Informative]", 
                               "Age Group [Adults]", 
                               "Trial Type [Medium]",
                               "Trial Type [Too-Complex]",
                               "Trial Type [Worst]",
                               "Question Condition [Legs]",
                               "Previous Quality Condition [Previously Informative] : Age Group [Adults]",
                               "Age Group [Adults] : Trial Type [Medium]",
                               "Age Group [Adults] : Trial Type [Too-Complex]",
                               "Age Group [Adults] : Trial Type [Worst]",
                               "Age Group [Adults] : Question Condition [Legs]",
                               "Intercept"))






##### is this a result of reuse, recombination, or both? #####

# is there an effect of whether a question matched the target on EIG?
# controlling for that, does the effect of exposure_cond remain (indicating recombination is doing something), or go away (indicating it's just reuse)?

eig.followup <- lmer(EIG ~ target_match + 
                exposure_cond_code +
                quality_cond_code * age_group_code + 
                trial_type * age_group_code + 
                question_cond_code * age_group_code +
                (1 | id),
              data = df_valid2)
summary(eig.followup)
anova(eig.followup, update(eig.followup, . ~ . - target_match))
anova(eig.followup, update(eig.followup, . ~ . - exposure_cond_code))
Confint(eig.followup)




## ----EXPLORATORY: Trial-to-trial reuse/recombination---------------------------------------------------------------------------------------------------


df_seq_consec <- df_valid2 %>% filter(!is.na(same_as_last))

nrow(df_seq_consec)
table(df_seq_consec$AgeGroup)


###### Does trial-to-trial reuse differ by age? #####

df_seq_consec$same_as_last <- as.factor(df_seq_consec$same_as_last)

reusetrials.kid <- glmer(same_as_last ~ age_mo_center + (1|id), 
                         data = df_seq_consec %>% filter(AgeGroup == "Kids"), family = "binomial")
summary(reusetrials.kid)
anova(reusetrials.kid, update(reusetrials.kid, . ~ . - age_mo_center))
 # effect of age, so bin age group


reusetrials.all <- glmer(same_as_last ~ AgeGroup_Split + (1|id), 
                         data = df_seq_consec, family = "binomial")
summary(reusetrials.all)
anova(reusetrials.all, update(reusetrials.all, . ~ . - AgeGroup_Split))
exp(Confint(reusetrials.all))

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




###### Does trial-to-trial recombination differ by age? #####
df_seq_consec_nonmatch <- df_seq_consec %>% filter(same_as_last == 0)


### dist_to_last (tree edit dist)
remixingtrials.kid <- lmer(dist_to_last_normed ~ age_mo_center + (1|id), 
                           data = df_seq_consec_nonmatch %>% filter(AgeGroup == "Kids"),
                           control = lmerControl(optimizer = "bobyqa"))
summary(remixingtrials.kid)
anova(remixingtrials.kid, update(remixingtrials.kid, .~.-age_mo_center)) # effect of age

remixingtrials.all <- lmer(dist_to_last_normed ~ AgeGroup_Split + (1|id), 
                           data = df_seq_consec_nonmatch, control = lmerControl(optimizer = "bobyqa"))
summary(remixingtrials.all)
anova(remixingtrials.all, update(remixingtrials.all, .~.-AgeGroup_Split)) # effect of age
Confint(remixingtrials.all)


##### make full regression table for supplement
m1 <- lme4::lmer(dist_to_last_normed ~ AgeGroup_Split + (1|id), 
                           data = df_seq_consec_nonmatch, control = lmerControl(optimizer = "bobyqa"))

stargazer(m1, type = "text",
          dep.var.labels = "Grammar-based similarity to most-similar previous question",
          ci = TRUE, digits = 2, digits.extra = 3, single.row = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001), omit.stat = c("aic", "bic", "ll", "n"), 
          covariate.labels = c("Age Group [7- to 8-year-olds]", 
                               "Age Group [9- to 10-year-olds]", "Age Group [Adults]", "Intercept"))



### sim_to_last (text similarity)
remixingtrials.kid <- lmer(sim_to_last_standard ~ age_mo_center + (1|id), 
                           data = df_seq_consec_nonmatch %>% filter(AgeGroup == "Kids"),
                           control = lmerControl(optimizer = "bobyqa"))
summary(remixingtrials.kid)
anova(remixingtrials.kid, update(remixingtrials.kid, .~.-age_mo_center)) # no effect of age

remixingtrials.all <- lmer(sim_to_last_standard ~ AgeGroup + (1|id), 
                           data = df_seq_consec_nonmatch)
summary(remixingtrials.all)
anova(remixingtrials.all, update(remixingtrials.all, .~.-AgeGroup)) # no effect of age
Confint(remixingtrials.all)



##### trial-to-trial reuse/recombination, comparison to null ########

sim_reuse_df <- read_csv("simulations/dqa_exposure_reuse.csv")
sim_remixing_df <- read_csv("simulations/dqa_exposure_remixing.csv")

# norm tree edit distance to same scale as true data
sim_remixing_df$treedist_mean_normed <- 1 - sim_remixing_df$treedist_mean/max(df_seq_consec$dist_to_last, na.rm = T)


# we've simulated 1000 new datasets, where the paired trials match the paired 
# set of trials seen by one participant
# each question and corresponding trial-to-trial statistics are sampled from ALL
# kids and adults who ever saw that question


reuse_rates <- tapply(df_seq_consec$same_as_last, df_seq_consec$AgeGroup_Split, function(x) {sum(x == 1)/length(x)})

#### null for reuse #####

# p-value: what proportion of our bootstrapped means are as or more extreme than the observed mean?
sum(sim_reuse_df %>% select(reuse_rate) >= reuse_rates["5- to 6-year-olds"])/1000
sum(sim_reuse_df %>% select(reuse_rate) >= reuse_rates["7- to 8-year-olds"])/1000
sum(sim_reuse_df %>% select(reuse_rate) >= reuse_rates["9- to 10-year-olds"])/1000
sum(sim_reuse_df %>% select(reuse_rate) >= reuse_rates["9- to 10-year-olds"])/1000

# this is the full range
quantile(sim_reuse_df %>% select(reuse_rate) %>% unlist(), 
         c(0, 1))
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
quantile(sim_remixing_df %>% select(treedist_mean_normed) %>% unlist(), 
         c(0, 1))
# true values are higher
mean_dist

# this is the full range of the null
quantile(sim_remixing_df %>% select(textsim_mean) %>% unlist(), 
         c(0, 1))
# true values are higher
mean_sim_standard





####### Supplement: alternative analyses of reuse ######

df_valid2_bypart <- df_valid2 %>% group_by(id, AgeGroup, AgeGroup_Split, age_mo_center, exposure_cond, exposure_cond_code,
                                           quality_cond, quality_cond_code, question_cond, question_cond_code) %>%
  summarize(n_match = sum(target_match == 1, na.rm = T))

### just kids
reuse.alt.kid.int <- lm(
  n_match ~ 
    exposure_cond_code * age_mo_center +
    quality_cond_code * age_mo_center +
    question_cond_code * age_mo_center ,
  data = df_valid2_bypart %>% filter(AgeGroup == "Kids"))
summary(reuse.alt.kid.int)
lmtest::lrtest(reuse.alt.kid.int, update(reuse.alt.kid.int, . ~ . - exposure_cond_code:age_mo_center))
# no interaction

reuse.alt.kid <- lm(
  n_match ~ 
    exposure_cond_code +
    quality_cond_code * age_mo_center +
    question_cond_code * age_mo_center ,
  data = df_valid2_bypart %>% filter(AgeGroup == "Kids"))
summary(reuse.alt.kid) ## main effect of age

### kids (split) vs. adults
reuse.alt.all.int <- lm(
  n_match ~ 
    exposure_cond_code * AgeGroup_Split +
    quality_cond_code * AgeGroup_Split +
    question_cond_code * AgeGroup_Split ,
  data = df_valid2_bypart)
summary(reuse.alt.all.int)
lmtest::lrtest(update(reuse.alt.all.int, .~.-exposure_cond_code:AgeGroup_Split),
               reuse.alt.all.int) # likelihood ratio test for lm
# no interaction

reuse.alt.all <- lm(
  n_match ~ 
    exposure_cond_code +
    quality_cond_code * AgeGroup_Split +
    question_cond_code * AgeGroup_Split ,
  data = df_valid2_bypart)
summary(reuse.alt.all)
lmtest::lrtest(update(reuse.alt.all, .~.-exposure_cond_code),
               reuse.alt.all)
Confint(reuse.alt.all)




####### exploratory for supplement: interaction between exposure and trial type #####
# why do people reuse less when questions are less informative?

#### tree dist
exp.trial.all.int1 <- lmer(target_dist_normed ~ 
                exposure_cond_code*trial_type_code +
                (exposure_cond_code + 
                   quality_cond_code +
                   trial_type_code + 
                   question_cond_code)*age_group_code + 
                (1 | id),
              data = df_nonmatch)
summary(exp.trial.all.int1)
anova(exp.trial.all.int1, update(exp.trial.all.int1, . ~ . - exposure_cond_code:trial_type_code))
Confint(exp.trial.all.int1)

emmeans(exp.trial.all.int1, revpairwise ~ exposure_cond_code | trial_type_code)
confint(emmeans(exp.trial.all.int1, revpairwise ~ exposure_cond_code | trial_type_code))

sjPlot::plot_model(exp.trial.all.int1, type = "pred", terms = c("trial_type_code", "exposure_cond_code"))


##### text-based similarity
exp.trial.all.int2 <- lmer(target_sim_standard ~ 
                exposure_cond_code*trial_type_code +
                (exposure_cond_code + 
                   quality_cond_code +
                   trial_type_code + 
                   question_cond_code)*age_group_code + 
                (1 | id),
              data = df_nonmatch)
summary(exp.trial.all.int2)
anova(exp.trial.all.int2, update(exp.trial.all.int2, . ~ . - exposure_cond_code:trial_type_code))
Confint(exp.trial.all.int2)

emmeans(exp.trial.all.int2, revpairwise ~ exposure_cond_code | trial_type_code)
confint(emmeans(exp.trial.all.int2, revpairwise ~ exposure_cond_code | trial_type_code))



sjPlot::plot_model(exp.trial.all.int2, type = "pred", terms = c("trial_type_code", "exposure_cond_code"))


