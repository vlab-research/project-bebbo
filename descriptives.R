library(readr)
library(dplyr)
library(tidyr)
library(cobalt)
library(glue)
library(stargazer)
library(psych)
library(stringr)
library(corrplot)
library(psych)
library (lattice)
library (nFactors)
library (lavaan)
library(moments)
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
    mutate("{x}" := mean(c_across(all_of(ss[[x]])),na.rm=TRUE)) #add columns summarizing constructs #how should we treat this if any one variable in the construct is NA?
}

#############################################################
# Descriptives
#############################################################

variable_cols<-unname(unlist(ss)) #all variables
construct_cols<-s #all constructs

#summarizing all variables
descrip_variable<-serbia%>%
  filter(endline==0)%>%
  select(all_of(variable_cols))%>%
  ungroup()%>%
  pivot_longer(cols=variable_cols)%>%
  group_by(name)%>%
  descriptives_prop()%>%
  arrange(desc(mean))%>%
  mutate_if(is.numeric, round, 2)

write_table(descrip_variable,'descriptives_variables',align=TRUE)

#summarizing construct variables
descrip_construct<-serbia%>%
  filter(endline==0)%>%
  select(all_of(construct_cols))%>%
  ungroup()%>%
  pivot_longer(cols=construct_cols)%>%
  group_by(name)%>%
  descriptives_summary()%>%
  arrange(desc(mean))%>%
  mutate_if(is.numeric, round, 2)

write_table(descrip_construct,'descriptives_constructs',align=TRUE)

constructs<-key[,c('Domain','Subdomain','construct_variable','variable','Grouping of constructs')]%>%
  mutate(Subdomain=case_when(is.na(Subdomain)~`Grouping of constructs`,TRUE~Subdomain),
         `Grouping of constructs`=NULL)%>%
  drop_na(construct_variable)%>%
  arrange(Domain,Subdomain,construct_variable,variable)

write_table(constructs,'constructs',align=TRUE)

#correlations
construct_cor<-serbia%>%
  filter(endline==0)%>%
  select(all_of(construct_cols))%>%
  lowerCor()%>%round(2)

construct_cor%>%corrplot(method='color')

# construct_cor[!lower.tri(construct_cor,diag=FALSE)]<-" "
# write_table(construct_cor,'construct_cor')  

