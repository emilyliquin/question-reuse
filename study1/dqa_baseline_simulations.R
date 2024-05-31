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



###################### permutation distribution for context-sensitivity ###########################

n_sim <- 1000

orig_trials <-  df_valid %>% select(trial_index, r_shape, r_legs, b_shape, b_legs, p_shape, p_legs)

# load script to get EIGs
source_python('../monsters_pyscripts/get_eigs.py')


boot_eig <- function(t){
  shuffled_trials <- orig_trials %>% slice_sample(n = nrow(orig_trials))
  shuffled_trials <- add_eigs(r_to_py(cbind(shuffled_trials, df_valid[c("AgeGroup", "question_program")])))
  return(tapply(shuffled_trials$EIG, shuffled_trials$AgeGroup, mean))
}

numCores <- detectCores()
numCores

set.seed(3984)
eig_sims <- mclapply(1:n_sim, boot_eig) %>% bind_rows()

sim_context_df <- data.frame(sim_number = 1:n_sim, EIG_kids = eig_sims$Kids, EIG_adults = eig_sims$Adults)

# cache data - uncomment to rewrite
# write_csv(sim_context_df, "simulations/dqa_baseline_context_eig.csv")


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

#function that we'll use to repeat this for the entire dataset (returns by-simulation reuse rate)
reuse_rep <- function(t){
  # apply to each row of dataset
  prev_matches <- lapply(1:nrow(to_match), sim_reuse) %>% unlist()
  
  # return proportion of trials w/ reuse
  p_reuse <- sum(prev_matches)/length(prev_matches)
  return(p_reuse)
}

numCores <- detectCores()
numCores

# simulate n_sim times
set.seed(194)
sim_reuse_rates <- mclapply(1:n_sim, reuse_rep) %>% unlist()

# convert to data frame for saving
sim_reuse_df <- data.frame(sim_number = 1:n_sim, reuse_rate = sim_reuse_rates)

# cache data - uncomment to rewrite
# write_csv(sim_reuse_df, "simulations/dqa_baseline_reuse.csv")

## ----simulating remixing--------------------------------------------------------------------------------------

source_python("../monsters_pyscripts/textsim_sbert_multi.py")

df_seq_consec_nonmatch <- df_seq_consec %>% filter(same_as_last != 1)

# structure to match
to_match_remixing <- df_seq_consec_nonmatch[,c("trial_index", "question_abstracted_idealized", "question_standard", "prev_trial_indices_remixing", "AgeGroup", "r_shape", "r_legs", "b_shape", "b_legs", "p_shape", "p_legs")]


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
  
  # double check that there's no reuse - analyses of remixing should exclude reuse
  stopifnot(!(row$question_abstracted_idealized %in% prev_trial_sample$question_abstracted_idealized))
  
  # get similarity between current question and sampled previous questions
  sims <- get_text_sim_multi(row$question_standard, prev_trial_sample$question_standard) %>% unlist()
  
  return(max(sims)) #most similar of previous trials
}

#function that we'll use to repeat this for the entire dataframe (returns by-simulation reuse rate)
remixing_rep <- function(t){
  print(t)
  # apply to each row of dataset
  prev_sims <- lapply(1:nrow(to_match_remixing), sim_remixing) %>% unlist()
  #for each simulated dataset, we want the mean similarity 
  return(mean(prev_sims))
}


# run simulations to generate permuted data -- can't parallelize this one without breaking
set.seed(21042)
sim_remixing_out <- lapply(1:n_sim, remixing_rep) %>% unlist()

sim_remixing_df <- data.frame(sim_number = 1:n_sim,
                              remixing_mean = sim_remixing_out)


# cache data
# write_csv(sim_remixing_df, "simulations/dqa_baseline_remixing.csv")
