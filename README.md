# Data and Analysis Scripts for "Seeking new information with old questions: Children and adults reuse and remix concepts from prior questions"

This repository includes data and analysis scripts to reproduce all results and figures in "Seeking new information with old questions: Children and adults reuse and remix concepts from prior questions"

It is organized into the following subdirectories:
- `monsters_pyscripts` contains utilities for representing questions as programs, computing a question's informational value (expected information gain), and computing the similarity between two questions. Much of this code is adapted from https://github.com/anselmrothe/EIG. See `monsters_pyscripts/README.md` for further information.
- `study1` contains data and analysis scripts for Study 1.
- `study2` contains data and analysis scripts for Study 2.
- `supplement_similarity` contains data and analysis scripts for the supplementary study used to select a metric of question similarity for our main analyses.

Some of the analysis scripts require the `monsters_pyscripts` utilities. Analysis scripts will automatically use the `environment.yml` file in the `monsters_pyscripts` folder to create a new conda environment with the necessary dependencies. This requires that you have conda installed on your system.


