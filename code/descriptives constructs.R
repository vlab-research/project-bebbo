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

#code<-"bebborsbaseserb" #filter for users who reach baseline
#code<-"bebborsendeng" #filter for users who reach endline
#code<-"bebborsfueng" #filter for users who reach followup

# 1. Serbia Survey Respondents
serbia <- read_csv("../data/raw/serbia/responses.csv") %>% #read Serbia responses file
  pick_serbia_resps(code="bebborsbaseserb") %>% #filter dataframe for Serbia respondents
  ind_treatment_control() %>% #create variables to indicate treatment, control
  ind_endline(country='serbia') %>% #create variables to indicate endline
  likert(likert_confs) %>% #convert likert construct variables to likert scale
  binarize(binary_confs) %>% #binarize construct variables
  group_by(userid) %>%
  mutate(gender = first(parent_gender, order_by = endline, na_rm = TRUE)) %>%
  ungroup()


# code<-"bebbobg2basebul" #filter for users who reach baseline
# code<-"bebbobgendeng" #filter for users who reach endline
# code<-"bebbobgfueng" #filter for users who reach followup

# 2. Bulgaria Survey Responses
bulgaria <- read_csv("../data/raw/bulgaria/responses.csv") %>% #read Bulgaria responses file
  pick_bulgaria_resps(code="bebbobg2basebul") %>% #filter dataframe for Bulgarian respondents
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

endline_flag=1 #0 - baseline, 1 - endline
country='bulgaria' #change to the country you want to run the analysis for - 'bulgaria' or 'serbia'

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
suffix<-ifelse(endline_flag==0,"_baseline","_endline")
pal <- colorRampPalette(c("red", "navy"))
dat%>%
  filter(endline==endline_flag)%>%
  select(all_of(construct_cols))%>%
  lowerCor()%>%
  corrplot(method = 'number',number.cex = 1,type = "lower",col = pal(2),tl.cex = 0.8)

dev.print(device = jpeg,
          filename=paste0('correlations_constructs_',country,'_',suffix,'.jpg'),
          width = 800,               
          height = 800)

# Writing tables


write_table(constructs,paste0('constructs',suffix),align=TRUE)
write_table(descrip_variable_likert,paste0('descriptives_likert_',country,suffix),align=TRUE)
write_table(descrip_variable_binary,paste0('descriptives_binary_',country,suffix),align=TRUE)
write_table(descrip_construct,paste0('descriptives_constructs_',country,suffix),align=TRUE)

