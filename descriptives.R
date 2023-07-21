library(readr)
library(dplyr)
library(tidyr)
library(cobalt)
library(glue)
library(stargazer)
library(psych)
library(stringr)
source("functions.R")
source("variable creation.R")


#############################################################
# Read Data
#############################################################

serbia <- read_csv("data/raw/serbia/responses.csv") %>% #read Serbia responses file
  pick_serbia_resps() %>% #filter dataframe for Serbia respondents
  ind_treatment_control() %>% #create variables to indicate treatment, control
  ind_endline() %>% #create variables to indicate endline
  likert(likert_confs) %>% #convert likert construct variables to likert scale
  binarize(binary_confs) %>% #binarize construct variables
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

# select_cols<-unname(unlist(ss))
select_cols<-names(ss)

descrip<-serbia%>%
  filter(endline==0)%>%
  select(all_of(select_cols))%>%
  ungroup()%>%
  pivot_longer(cols=select_cols)%>%
  group_by(name)%>%
  summarise(mean=mean(value,na.rm=TRUE),
            median=median(value,na.rm=TRUE),
            min=min(value,na.rm=TRUE),
            max=max(value,na.rm=TRUE),
            sd=sd(value,na.rm=TRUE))

descrip<-descrip%>%
  merge(key[,c('construct_variable','Subdomain','Domain','Grouping of constructs')],by.x = 'name',by.y='construct_variable',all.x = TRUE)%>%
  distinct()

descrip<-descrip%>%
  mutate(Subdomain=case_when(is.na(Subdomain) ~ `Grouping of constructs`, TRUE ~ Subdomain))%>%
  select(c('Subdomain','mean','median','min','max','sd'))

write_table(descrip,'descriptives1')

#correlations
lower_cor<-serbia%>%
  filter(endline==0)%>%
  select(all_of(select_cols))%>%
  select(-caregiver_well_being)%>%
  lowerCor()

#############################################################
# Reliability Analysis
#############################################################

serbia%>%
  filter(endline==0)%>%
  select(all_of(select_cols))%>%alpha()

dat<-serbia%>%filter(endline==0)%>%select(all_of(select_cols))
pca <- pca(dat, nfactors = 3)
