library(tidyverse)
library(reticulate)

setwd(this.path::here())

# set up conda environment for python scripts
if(!condaenv_exists(envname = "dqashare")){
  conda_create(envname = "dqashare",
               environment = "../monsters_pyscripts/environment.yml")
}
use_condaenv(condaenv = "dqashare", required = T)

# read data
df2 <- read_csv("data/dqa_exposure_cleandata.csv")

df2 <- df2 %>% mutate(
  AgeGroup = as.factor(AgeGroup),
  id = as.factor(id),
  trial_index = as.factor(trial_index),
  question_cond = as.factor(question_cond),
  quality_cond = as.factor(quality_cond),
  exposure_cond = as.factor(exposure_cond),
  trial_type = as.factor(trial_type),
)

df2$AgeGroup <- relevel(df2$AgeGroup, ref = "Kids")
df2$age_mo_center <- as.vector(scale(df2$age_mo, scale = FALSE))

df2$AgeGroup_Split <- as.factor(ifelse(df2$age_yr %in% c(5, 6),
                                       "5- to 6-year-olds",
                                       ifelse(df2$age_yr %in% c(7, 8),
                                              "7- to 8-year-olds",
                                              ifelse(df2$age_yr %in% c(9, 10),
                                                     "9- to 10-year-olds", "Adults"
                                              )
                                       )
))




####################### get match to target questions (reuse) ####################### 

df2$match_head <- as.factor(ifelse(df2$question_abstracted_idealized == "(++ (map (lambda x0 (== (shape x0) s)) (set b r p)))", 1, 0))
df2$match_legs <- as.factor(ifelse(df2$question_abstracted_idealized == "(++ (map (lambda x0 (legs x0)) (set b r p)))", 1, 0))


df2$target_match <- as.factor(ifelse(df2$question_cond == "heads", as.character(df2$match_head), 
                                     ifelse(df2$question_cond == "legs", as.character(df2$match_legs), NA)))


#check that this worked right
table(df2$target_match, df2$question_cond, df2$question_program == "(++ (map (lambda x0 (== (shape x0) Square)) (set b r p)))" | 
        df2$question_program == "(++ (map (lambda x0 (== (shape x0) Circle)) (set b r p)))")

table(df2$target_match, df2$question_cond, df2$question_program == "(++ (map (lambda x0 (legs x0)) (set b r p)))")


####################### compute similarity to target questions (remixing) ####################### 


#### text similarity script
source_python("../monsters_pyscripts/textsim_sbert.py")

#### tree edit distance script
source_python("../monsters_pyscripts/distance.py")

df2$target_sim_standard <- NA
df2$target_dist <- NA

for (i in 1:nrow(df2)) {
  print(i)
  target_question <- ifelse(df2[i, "question_cond"] == "heads", "How many monsters have a square head?",
                            ifelse(df2[i, "question_cond"] == "legs", "How many legs do all the monsters have combined together?",
                                   NA))
  target_program <- ifelse(df2[i, "question_cond"] == "heads", "(++ (map (lambda x0 (== (shape x0) Square)) (set b r p)))",
                           ifelse(df2[i, "question_cond"] == "legs", "(++ (map (lambda x0 (legs x0)) (set b r p)))",
                                  NA))
  if (!is.na(df2[i, "question_program"])) {
    df2[i, "target_sim_standard"] <- get_text_sim(as.character(target_question), as.character(df2[i, "question_standard"]))
    df2[i, "target_dist"] <- get_sim(as.character(target_program), as.character(df2[i, "question_program"]))
  }
}


hist(df2$target_sim_standard)
hist(df2$target_dist)


######################## reuse and remixing from previous trials ##########################


# create variables
df2$same_as_last <- NA
df2$sim_to_last_standard <- NA
df2$prev_trial_indices_reuse <- NA
df2$prev_trial_indices_remixing <- NA



# loop through each trial, calculate relevant variables by comparison to previous trials (same id number, trial is all previous trials)
for(i in 1:nrow(df2)){
  row <- df2[i,]
  if(row$trial != 1 & !is.na(row$question_abstracted_idealized)){
    
    last_trial <- df2[df2$id == row$id & df2$trial %in% (1:(row$trial-1)),] #all previous trials
    last_trial <- last_trial %>% filter(!is.na(question_abstracted_idealized)) # only with valid questions
    
    if(nrow(last_trial) >= 1){
      last_trial_nonreused <- last_trial %>% filter(question_abstracted_idealized != row$question_abstracted_idealized) #remove matches
      
      df2[i, "same_as_last"] <- ifelse(nrow(last_trial) > nrow(last_trial_nonreused), 1, 0) #if we excluded any rematches, that was reuse
      
      if(nrow(last_trial_nonreused) >= 1){ #if there are any remaining questions
        last_trial_nonreused <- last_trial_nonreused %>% rowwise() %>%
          mutate(textsimstandard = get_text_sim(question_standard, row$question_standard)) #get similarity to current question
        
        df2[i, "sim_to_last_standard"] <- max(last_trial_nonreused$textsimstandard) #most similar
        
      }
      
      df2[i, "prev_trial_indices_reuse"] <- paste0(last_trial$trial_index, collapse = ",")
      df2[i, "prev_trial_indices_remixing"] <- paste0(last_trial_nonreused$trial_index, collapse = ",")
    }
  }
}



############# clean and save data ###############

colnames(df2)
df2 <- df2 %>% relocate(question_standard, .after=question)
df2 <- df2 %>% relocate(age_mo_center, .after=age_mo)
df2 <- df2 %>% relocate(AgeGroup_Split, .after=AgeGroup)


# uncomment to rewrite data
# write_csv(df2, "data/dqa_exposure_preppeddata.csv")


