library(readr)
library(tidyr)
library(cobalt)
library(glue)
library(stargazer)
source("functions.R")
source("variable creation.R") #requires functions in functions.R
setwd("~/v-lab/project-bebbo/")

#############################################################
# Read Data
#############################################################

serbia <- read_csv("data/raw/serbia/responses.csv") %>% #read Serbia responses file
  pick_serbia_resps() %>% #filter dataframe for Serbia respondents
  ind_treatment_control() %>% #create variables to indicate treatment, control
  ind_endline() %>% #create variables to indicate endline
  binarize(binary_confs) %>% #binarize construct variables using map created above
  group_by(userid) %>%
  mutate(gender = first(parent_gender, order_by = endline, na_rm = TRUE)) %>%
  ungroup()

for (x in names(ss)) {
  serbia <- serbia %>%
    rowwise() %>%
    mutate("{x}" := mean(c_across(all_of(ss[[x]])))) #add columns summarizing constructs
}


#############################################################
# Descriptives
#############################################################




