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
df <- read_csv("data/dqa_baseline_cleandata.csv")


df <- df %>% mutate(AgeGroup = as.factor(AgeGroup),
                    id = as.factor(id),
                    trial_index = as.factor(trial_index))

df$AgeGroup <- relevel(df$AgeGroup, ref = "Kids")
df$age_mo_center <- as.vector(scale(df$age_mo, scale = FALSE))

df$AgeGroup_Split <- ifelse(df$age_yr %in% c(5, 6),
                            "5- to 6-year-olds",
                            ifelse(df$age_yr %in% c(7, 8),
                                   "7- to 8-year-olds",
                                   ifelse(df$age_yr %in% c(9, 10),
                                          "9- to 10-year-olds", "Adults")))




####################### question complexity ########################


df$question_cleaned <- str_remove_all(df$question_program, "\\(")
df$question_cleaned <- str_remove_all(df$question_cleaned, "\\)")

df_splitqs <- str_split(df$question_cleaned, " ")

# get rid of lambda because lambda always has to go with map -- so it's like double counting every time you use this, which
# seems unfair?
removes <- c("1", "2", "3", "Square", "Circle", "b", "r", "p", "x0", "lambda")

df_splitqs <- df_splitqs %>% map(~ .[!(. %in% removes)])
df$n_operations <- unlist(df_splitqs %>% map(~ length(.)))
df$n_operations <- ifelse(is.na(df$question_program), NA, df$n_operations)


df$n_unique_operations <- unlist(df_splitqs %>% map(~ length(unique(.))))
df$n_unique_operations <- ifelse(is.na(df$question_program), NA, df$n_unique_operations)

df <- df %>% select(!question_cleaned)


######################## reuse and remixing from previous trials ##########################


#### text similarity script
source_python("../monsters_pyscripts/textsim_sbert.py")

#### tree edit distance script
source_python("../monsters_pyscripts/distance.py")


# create variables
df$same_as_last <- NA
df$sim_to_last_standard <- NA
df$dist_to_last <- NA
df$prev_trial_indices_reuse <- NA
df$prev_trial_indices_remixing <- NA



# loop through each trial, calculate relevant variables by comparison to previous trials (same id number, trial is all previous trials)
for(i in 1:nrow(df)){
  row <- df[i,]
  # skip first trial (no previous trials) and any trial where the question wasn't valid
  if(row$trial != 1 & !is.na(row$question_abstracted_idealized)){
    
    last_trial <- df[df$id == row$id & df$trial %in% (1:(row$trial-1)),] # get all previous trials
    last_trial <- last_trial %>% filter(!is.na(question_abstracted_idealized)) # only those with valid questions
    
    if(nrow(last_trial) >= 1){
      last_trial_nonreused <- last_trial %>% filter(question_abstracted_idealized != row$question_abstracted_idealized) #remove matches
      
      df[i, "same_as_last"] <- ifelse(nrow(last_trial) > nrow(last_trial_nonreused), 1, 0) #if we excluded any rematches, there was reuse
      
      if(nrow(last_trial_nonreused) >= 1){ #if there are any remaining questions
        last_trial_nonreused <- last_trial_nonreused %>% rowwise() %>%
          mutate(textsimstandard = get_text_sim(question_standard, row$question_standard),
                 disttolast = get_sim(as.character(question_program), as.character(row$question_program))) #get similarity to current question
        
        df[i, "sim_to_last_standard"] <- max(last_trial_nonreused$textsimstandard) #most similar
        df[i, "dist_to_last"] <- min(last_trial_nonreused$disttolast) #most similar (min dist)
        
      }
      # save which trials were valid for analysis of reuse and remixing
      df[i, "prev_trial_indices_reuse"] <- paste0(last_trial$trial_index, collapse = ",")
      df[i, "prev_trial_indices_remixing"] <- paste0(last_trial_nonreused$trial_index, collapse = ",")
    }
  }
}

############# clean and save data ###############

colnames(df)
df <- df %>% relocate(question_standard, .after=question)
df <- df %>% relocate(age_mo_center, .after=age_mo)
df <- df %>% relocate(AgeGroup_Split, .after=AgeGroup)


# uncomment to rewrite data
write_csv(df, "data/dqa_baseline_preppeddata.csv")

