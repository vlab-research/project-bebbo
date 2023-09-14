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
serbia <- read_csv("../data/raw/serbia/responses.csv") %>% #read Serbia responses file
  pick_serbia_resps(stage="end") %>% #filter dataframe for Serbia respondents
  ind_treatment_control() %>% #create variables to indicate treatment, control
  ind_endline(country='serbia') %>% #create variables to indicate endline
  likert(likert_confs) %>% #convert likert construct variables to likert scale
  binarize(binary_confs) %>% #binarize construct variables
  group_by(userid) %>%
  mutate(gender = first(parent_gender, order_by = endline, na_rm = TRUE)) %>%
  ungroup()

# 2. Bulgaria Survey Responses
bulgaria <- read_csv("../data/raw/bulgaria/responses.csv") %>% #read Bulgaria responses file
  pick_bulgaria_resps(stage="end") %>% #filter dataframe for Bulgarian respondents
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
# Descriptives
#############################################################
# In this section, we perform the following analyses for baseline vs endline and across both countries, serbia and bulgaria
# 1. Baseline Descriptives - Serbia
# 2. Baseline Descriptives - Bulgaria
# 3. Endline Descriptives - Serbia
# 4. Endline Descriptives - Bulgaria
# 5. Correlations - Serbia
# 6. Correlations - Bulgaria

endline_flag=0 #0 - baseline, 1 - endline
country='serbia' #change to the country you want to run the analysis for - 'bulgaria' or 'serbia'

if(country=='serbia')
{dat<-serbia
}else if(country=='bulgaria')
{dat<-bulgaria}

# 1. Baseline Descriptives - Serbia
# 2. Baseline Descriptives - Bulgaria
# 3. Endline Descriptives - Serbia
# 4. Endline Descriptives - Bulgaria

variable_cols<-unname(unlist(ss)) #all variables
construct_cols<-s #all constructs

constructs<-key[,c('Domain','Subdomain','construct_variable','variable','Grouping of constructs')]%>%
  mutate(Subdomain=case_when(is.na(Subdomain)~`Grouping of constructs`,TRUE~Subdomain),
         `Grouping of constructs`=NULL)%>%
  drop_na(construct_variable)%>%
  arrange(Domain,Subdomain,construct_variable,variable)

# 1-4a. Likert Variables - Most granular summary of likert variables
descrip_variable_likert<-dat%>%
  filter(endline==endline_flag)%>%
  select(all_of(likert_cols))%>%
  descriptives(type='likert')%>%
  merge(constructs[,c('variable','Subdomain')],by.x = 'name',by.y = 'variable',all.x=TRUE)%>%
  relocate(Subdomain,.before='name')%>%
  arrange(Subdomain)

# 1-4b. Binary Variables - Most granular summary of likert variables
descrip_variable_binary<-dat%>%
  filter(endline==endline_flag)%>%
  select(all_of(variable_cols))%>%
  select(!likert_cols)%>%
  descriptives(type='binary')%>%
  merge(constructs[,c('variable','Subdomain')],by.x = 'name',by.y = 'variable',all.x=TRUE)%>%
  relocate(Subdomain,.before='name')%>%
  arrange(Subdomain)

# 1-4c. Construct Variables
descrip_construct<-dat%>%
  filter(endline==endline_flag)%>%
  select(all_of(construct_cols))%>%
  descriptives(type='summary')%>%
  merge(constructs[,c('construct_variable','Subdomain')],by.x = 'name',by.y = 'construct_variable',all.x=TRUE)%>%
  distinct()%>%
  relocate(Subdomain,.before='name')%>%
  arrange(Subdomain)

# 5. Correlations - Serbia
# 6. Correlations - Bulgaria

construct_cor<-dat%>%
  filter(endline==endline_flag)%>%
  select(all_of(construct_cols))%>%
  lowerCor()%>%round(2)

# Writing tables

suffix<-ifelse(endline_flag==0,"_baseline","_endline")
write_table(constructs,paste0('constructs',suffix),align=TRUE)
write_table(descrip_variable_likert,paste0('descriptives_likert_',country,'_',suffix),align=TRUE)
write_table(descrip_variable_binary,paste0('descriptives_binary_',country,'_',suffix),align=TRUE)
write_table(descrip_construct,paste0('descriptives_constructs_',country,'_',suffix),align=TRUE)
dev.print( device = jpeg,
           filename = paste0('correlations_constructs_',country,'_',suffix,'.jpg'), 
           width = 480,               
           height = 300               
           )

#############################################################
# Number of Respondents
#############################################################

# respondents_endline<-dat%>%
#   mutate(age_flag=case_when(child_age=="0 to 6 months"~"0-2",
#                             child_age=="6 to 12 months"~"0-2",
#                             child_age=="12 to 24 months"~"0-2",
#                             child_age=="2 to 4 years"~"2-6",
#                             child_age=="4 to 6 years"~"2-6"))%>%
#   group_by(endline,age_flag)%>%
#   summarise(count=table(age_flag))%>%
#   mutate(endline=recode(endline,`0`="Baseline",`1`="Endline"))%>%
#   pivot_wider(id_cols = endline,id_expand = FALSE,names_from=age_flag,names_expand=TRUE,values_from = count)
# 
# write_table(respondents_endline,'respondents_endline')
# 
# respondents<-dat%>%
#   mutate(age_flag=case_when(child_age=="0 to 6 months"~"0-2",
#                             child_age=="6 to 12 months"~"0-2",
#                             child_age=="12 to 24 months"~"0-2",
#                             child_age=="2 to 4 years"~"2-6",
#                             child_age=="4 to 6 years"~"2-6"))%>%
#   ungroup()%>%
#   select(all_of(variable_cols))%>%
#   pivot_longer(everything())%>%
#   group_by(name)%>%
#   summarise(respondents=sum(!is.na(value)),value=NULL)
# 
# write_table(respondents,'respondents')
# 
# respondents_byage<-dat%>%
#   mutate(age_flag=case_when(child_age=="0 to 6 months"~"0-2",
#                             child_age=="6 to 12 months"~"0-2",
#                             child_age=="12 to 24 months"~"0-2",
#                             child_age=="2 to 4 years"~"2-6",
#                             child_age=="4 to 6 years"~"2-6"))%>%
#   ungroup()%>%
#   select(all_of(c(variable_cols,'age_flag','endline')))%>%
#   pivot_longer(cols=variable_cols)%>%
#   group_by(name,age_flag)%>%
#   summarise(respondents=sum(!is.na(value)),value=NULL)%>%
#   pivot_wider(id_cols = name ,id_expand = FALSE,names_from=age_flag,names_expand=TRUE,values_from = respondents)%>%
#   mutate(total=`0-2`+`2-6`,
#          `NA`=NULL)
# 
# write_table(respondents_byage,'respondents_childage')
#   
# 
