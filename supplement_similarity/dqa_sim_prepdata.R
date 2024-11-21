library(tidyverse)
library(reticulate)

setwd(this.path::here())

# set up conda environment for python scripts
if(!condaenv_exists(envname = "dqashare")){
  conda_create(envname = "dqashare",
               environment = "../monsters_pyscripts/environment.yml")
}
use_condaenv(condaenv = "dqashare", required = T)

#get data
fulldf <- read_csv("data/dqa_similarity_clean.csv")

##### get similarity functions -- tree edit distance, text similarity, and intersecting functions/arguments  ######

source_python("../monsters_pyscripts/distance.py")

source_python("../monsters_pyscripts/textsim_sbert.py")

get_intersect <- function(qp1, qp2){
  qp1_split <- str_split(str_remove_all(qp1, pattern = "[\\(]|[\\)]"), pattern = " ") %>% map(~ unique(.))
  qp2_split <- str_split(str_remove_all(qp2, pattern = "[\\(]|[\\)]"), pattern = " ") %>% map(~ unique(.))
  
  arguments_q1 <- qp1_split %>% map(~ grep("^b$|^r$|^p$|Square|Circle|\\d", ., value = T))
  functions_q1 <- qp1_split %>% map(~ grep("^b$|^r$|^p$|Square|Circle|\\d", ., value = T, invert = T))
  arguments_q2 <- qp2_split %>% map(~ grep("^b$|^r$|^p$|Square|Circle|\\d", ., value = T))
  functions_q2 <- qp2_split %>% map(~ grep("^b$|^r$|^p$|Square|Circle|\\d", ., value = T, invert = T))
  
  # for arguments, filter out x0
  arguments_q1 <- arguments_q1 %>% map(~ .[. != "x0"])
  arguments_q2 <- arguments_q2 %>% map(~ .[. != "x0"])
  
  # for functions, filter out lambda
  functions_q1 <- functions_q1 %>% map(~ .[. != "lambda"])
  functions_q2 <- functions_q2 %>% map(~ .[. != "lambda"])
  
  intersecting_arguments <- map2(arguments_q1, arguments_q2, \(x, y) length(intersect(x, y))/length(y)) %>% unlist()
  intersecting_functions <- map2(functions_q1, functions_q2, \(x, y) length(intersect(x, y))/length(y)) %>% unlist()
  
  return(c(intersecting_arguments, intersecting_functions))
}

fulldf$dist_tree <- NA
fulldf$sim_text <- NA
fulldf$sim_text_standard <- NA
fulldf$intersecting_functions12 <- NA
fulldf$intersecting_functions21 <- NA
fulldf$intersecting_arguments12 <- NA
fulldf$intersecting_arguments21 <- NA

for(i in 1:nrow(fulldf)){
  print(i)
  fulldf[i, "dist_tree"] <- get_sim(as.character(fulldf[i, "q1_program"]), as.character(fulldf[i, "q2_program"]))
  fulldf[i, "sim_text"] <- get_text_sim(as.character(fulldf[i, "q1"]), as.character(fulldf[i, "q2"]))
  fulldf[i, "sim_text_standard"] <- get_text_sim(as.character(fulldf[i, "q1_standard"]), as.character(fulldf[i, "q2_standard"]))
  
  intersect_both12 <- get_intersect(as.character(fulldf[i, "q1_program"]), as.character(fulldf[i, "q2_program"]))
  fulldf[i, "intersecting_functions12"] <- intersect_both12[2]
  fulldf[i, "intersecting_arguments12"] <- intersect_both12[1]
  
  intersect_both21 <- get_intersect(as.character(fulldf[i, "q2_program"]), as.character(fulldf[i, "q1_program"]))
  fulldf[i, "intersecting_functions21"] <- intersect_both21[2]
  fulldf[i, "intersecting_arguments21"] <- intersect_both21[1]
  
}


# uncomment to rewrite
write_csv(fulldf, "data/dqa_similarity_preppeddata.csv")

