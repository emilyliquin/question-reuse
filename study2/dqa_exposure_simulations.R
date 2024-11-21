library(tidyverse)
library(reticulate)
library(parallel)

setwd(this.path::here())

# set up conda environment for python scripts
if(!condaenv_exists(envname = "dqashare")){
  conda_create(envname = "dqashare",
               environment = "../monsters_pyscripts/environment.yml")
}
use_condaenv(condaenv = "dqashare", required = T)


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
df_valid <- df_noskips %>% filter(!is.na(question_program))




###################### bootstrap distribution for reuse/remixing ###########################

n_sim <- 1000

df_seq_consec <- df %>% filter(!is.na(same_as_last))


# ----simulating reuse--------------------------------------------------------------------------------------

# structure to match
to_match <- df_seq_consec[,c("trial_index", "question_abstracted_idealized", "prev_trial_indices_reuse", "AgeGroup", "r_shape", "r_legs", "b_shape", "b_legs", "p_shape", "p_legs")]

#function to match trial structure w/ random previous questions and check reuse row by row
sim_reuse <- function(i){
  # for a given row i
  row <- to_match[i,]
  # get the trial indices for all prior reuse-eligible trials
  prev_trial_indices <- str_split(row$prev_trial_indices_reuse, ",") %>% unlist()
  
  # then use full data to resample a random question from each of those trial indices
  prev_trial_sample <- df_valid %>%
    filter(trial_index %in% prev_trial_indices) %>%
    group_by(trial_index) %>%
    slice_sample(n = 1) %>%
    ungroup()
  
  # test whether the current-trial question matches any of the previous-trial questions
  return(row$question_abstracted_idealized %in% prev_trial_sample$question_abstracted_idealized)
}

#function that we'll use to repeat this for the entire dataframe (returns by-simulation reuse rate)
reuse_rep <- function(t){
  # apply to each row of dataset
  prev_matches <- lapply(1:nrow(to_match), sim_reuse) %>% unlist()
  
  # return proportion of trials w/ reuse
  p_reuse <- sum(prev_matches)/length(prev_matches)
  return(p_reuse)
}

numCores <- detectCores()
numCores

set.seed(231092)
sim_reuse_rates <- mclapply(1:n_sim, reuse_rep) %>% unlist()

sim_reuse_df <- data.frame(sim_number = 1:n_sim, reuse_rate = sim_reuse_rates)

# cache data - uncomment to rewrite
# write_csv(sim_reuse_df, "simulations/dqa_exposure_reuse.csv")


## ----simulating remixing--------------------------------------------------------------------------------------

source_python("../monsters_pyscripts/textsim_sbert_multi.py")
source_python("../monsters_pyscripts/distance.py")

n_sim <- 1000

df_seq_consec_nonmatch <- df_seq_consec %>% filter(same_as_last != 1)

# structure to match
to_match_remixing <- df_seq_consec_nonmatch[,c("trial_index", "question_program", "question_abstracted_idealized", "question_standard", "prev_trial_indices_remixing", "AgeGroup", "r_shape", "r_legs", "b_shape", "b_legs", "p_shape", "p_legs")]


#function to match trial structure w/ random previous questions and check remixing row by row
sim_remixing <- function(i){
  # for a given row i
  row <- to_match_remixing[i,]
  # get the trial indices for all prior remixing-eligible trials
  prev_trial_indices <- str_split(row$prev_trial_indices_remixing, ",") %>% unlist()
  
  # then use full data to resample a random question from each of those trial indices
  # with the additional criteria that previous questions aren't the same question program as current question
  prev_trial_sample <- df_valid %>%
    filter(trial_index %in% prev_trial_indices & question_abstracted_idealized != row$question_abstracted_idealized) %>%
    group_by(trial_index) %>%
    slice_sample(n = 1) %>%
    ungroup()
  
  # check that there's no reuse - analyses of remixing should exclude reuse
  stopifnot(!(row$question_abstracted_idealized %in% prev_trial_sample$question_abstracted_idealized))
  
  # get similarity between current question and sampled previous questions
  sims <- get_text_sim_multi(row$question_standard, prev_trial_sample$question_standard) %>% unlist()
  dists <- get_sim_multi(row$question_program, prev_trial_sample$question_program) %>% unlist()
  
  return(c(max(sims), min(dists))) #most similar of previous trials
}

#function that we'll use to repeat this for the entire dataframe (returns by-simulation reuse rate)
remixing_rep <- function(t){
  print(t)
  prev_sims <- lapply(1:nrow(to_match_remixing), sim_remixing)
  sims <- map(prev_sims, 1) %>% unlist()
  dists <- map(prev_sims, 2) %>% unlist()
  #for each simulated dataset, we want the mean similarity 
  return(c(mean(sims), mean(dists)))
}


# run simulations to generate permuted data -- can't parallelize this one without breaking
set.seed(908921)
sim_remixing_out <- lapply(1:n_sim, remixing_rep)

sim_remixing_df <- data.frame(sim_number = 1:n_sim,
                              textsim_mean = map(sim_remixing_out, 1) %>% unlist(),
                              treedist_mean = map(sim_remixing_out, 2) %>% unlist())


# cache data
# write_csv(sim_remixing_df, "simulations/dqa_exposure_remixing.csv")

