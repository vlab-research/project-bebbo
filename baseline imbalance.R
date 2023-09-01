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
# In this section, we read and perform some pre-processing for the datasets we will require for this analysis

# 1. Serbia Survey Respondents
serbia <- read_csv("data/raw/serbia/responses.csv") %>% #read Serbia responses file
  pick_serbia_resps() %>% #filter dataframe for Serbia respondents
  ind_treatment_control() %>% #create variables to indicate treatment, control
  ind_endline(country='serbia') %>% #create variables to indicate endline
  likert(likert_confs) %>% #convert likert construct variables to likert scale
  binarize(binary_confs) %>% #binarize construct variables
  group_by(userid) %>%
  mutate(gender = first(parent_gender, order_by = endline, na_rm = TRUE)) %>%
  ungroup()

# 2. Bulgaria Survey Responses
bulgaria <- read_csv("data/raw/bulgaria/responses.csv") %>% #read Bulgaria responses file
  pick_bulgaria_resps() %>% #filter dataframe for Bulgarian respondents
  ind_treatment_control() %>% #create variables to indicate treatment, control
  ind_endline(country='bulgaria') %>% #create variables to indicate endline
  likert(likert_confs) %>% #convert likert construct variables to likert scale
  binarize(binary_confs) %>% #binarize construct variables
  group_by(userid) %>%
  mutate(gender = first(parent_gender, order_by = endline, na_rm = TRUE)) %>%
  ungroup()

for (x in names(ss)) {
  serbia <- serbia %>%
    rowwise() %>%
    mutate("{x}" := mean(c_across(all_of(ss[[x]])),na.rm=TRUE)) 
  
  bulgaria <- bulgaria %>%
    rowwise() %>%
    mutate("{x}" := mean(c_across(all_of(ss[[x]])),na.rm=TRUE))
}

#############################################################
# Paired t-tests
#############################################################

variable_cols<-unname(unlist(ss)) #all variables
construct_cols<-s #all constructs
demo_cols<-c('parent_age')
all_cols<-c(variable_cols,construct_cols,demo_cols)

#initialize a matrix to store results
t.test.serbia<-matrix(nrow=length(all_cols),ncol=6)
#baseline treated
treated.serbia<-serbia%>%filter(endline==0 && treatment=='treated')
#baseline control
control.serbia<-serbia%>%filter(endline==0 && treatment=='control')

i=1
for(col in all_cols){
test<-t.test(x=treated.serbia[[col]],y=control.serbia[[col]],mu=0,paired=FALSE,var.equal=TRUE)
t.test.serbia[i,1]  <-col
t.test.serbia[i,2:6]<-round(cbind(test$statistic,test$p.value,test$stderr,test$conf.int[1],test$conf.int[2]),3)
i=i+1
rm(test)
}

t.test.serbia<-as.data.table(t.test.serbia)
colnames(t.test.serbia)<-c('variable','test statistic','p-val','std.error','conf.int lower','conf.int upper')

#initialize a matrix to store results
t.test.bulgaria<-matrix(nrow=length(all_cols),ncol=6)
#baseline treated
treated.bulgaria<-bulgaria%>%filter(endline==0 && treatment=='treated')
#baseline control
control.bulgaria<-bulgaria%>%filter(endline==0 && treatment=='control')

i=1
for(col in all_cols){
  print(col)
  test<-t.test(x=treated.bulgaria[[col]],y=control.bulgaria[[col]],mu=0,paired=FALSE,var.equal=TRUE)
  t.test.bulgaria[i,1]  <-col
  t.test.bulgaria[i,2:6]<-round(cbind(test$statistic,test$p.value,test$stderr,test$conf.int[1],test$conf.int[2]),3)
  i=i+1
  rm(test)
}

t.test.bulgaria<-as.data.table(t.test.bulgaria)
colnames(t.test.bulgaria)<-c('variable','test statistic','p-val','std.error','conf.int lower','conf.int upper')

