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
  ind_endline(country='serbia') #create variables to indicate endline

# 2. Bulgaria Survey Responses

bulgaria <- read_csv("data/raw/bulgaria/responses.csv") %>% #read Bulgaria responses file
  ind_endline(country='bulgaria') #create variables to indicate endline

# 3. Timing of events data - we use this to find the time that users are asked to download the app

serbia_long<-fread("data/raw/serbia/serbia-long.csv")%>%
  filter(question_ref=='download_confirm_treatment')%>%
  select(userid,timestamp)

bulgaria_long<-fread("data/raw/bulgaria/bulgaria-long.csv")%>%
  filter(question_ref=='download_confirm_treatment')%>%
  select(userid,timestamp)

download_time<-rbind(serbia_long,bulgaria_long)

#############################################################
# App Usage Over Time
#############################################################

# The app metrics that we are interested in and need to create events for accordingly
# # # installed
# # # home opened
# # # attrition
# # # usage time

# 4a. App Usage Data aggregated by week
# This dataset only contains respondents that downloaded the app and have events logged.
# For each respondent, we get the total weekly home opens, time used in the week (days), and number of events logged (usage_count)

app_usage_weekly <- fread("data/raw/bq-results-20230824.csv") %>%
  merge(download_time,by.x='facebook_id',by.y='userid',all.x=TRUE,all.y=FALSE)%>%
  mutate(event_timestamp = parse_bq_date(event_timestamp), # event timestamp
         start_week = week(timestamp), # respondent's start week - week of respondent being asked to download the app
         week_since_start = round(difftime(event_timestamp,timestamp,units = "week"))) %>% # weeks since the start week (event timestamp - start time)
  group_by(facebook_id,week_since_start)%>% #respondent's activity by weeks since joining
  summarize(start_week=first(start_week),
            week_since_start=first(week_since_start),
            facebook_id = first(facebook_id),
            session_starts = sum(event_name == "session_start"),
            screen_views = sum(event_name == "screen_view"),
            user_engagements = sum(event_name == "user_engagement"),
            home_opens = sum(event_name == "Home_opened"), #total weekly home opens
            milestones_tracked = sum(event_name == "child_milestone_tracked"), 
            time_used = difftime(max(event_timestamp),min(event_timestamp),units = 'days'), #days used in the week
            usage_count=length(event_timestamp), #number of events logged in the week
            opened=ifelse(sum(grepl('_opened',event_name))>0,1,0)) #opened the app in the week

# 4b. App Usage Data weekly trends
weekly_aggregates<-app_usage_weekly%>%
  group_by(week_since_start)%>%
  summarise(home_opens=mean(home_opens), #average number of home opens in the week
            respondents=n_distinct(facebook_id), #count of respondents
            time_used=mean(time_used), #average number of days used
            usage_count=mean(usage_count), #number of events logged in the week
            opened=sum(opened))%>% #proportion opened
  filter(week_since_start>=0)
  
write_table(weekly_aggregates,'weekly_aggregates')


#############################################################
# Attrition and App Usage
#############################################################

app_usage <- fread("data/raw/bq-results-20230824.csv") %>%
  merge(download_time,by.x='facebook_id',by.y='userid',all.x=TRUE,all.y=FALSE)%>%
  mutate(event_timestamp = parse_bq_date(event_timestamp),
         start_week = week(timestamp)) %>%
  group_by(facebook_id) %>% 
  summarize(
    start_week=first(start_week),
    facebook_id = first(facebook_id),
    session_starts = sum(event_name == "session_start"),
    screen_views = sum(event_name == "screen_view"),
    user_engagements = sum(event_name == "user_engagement"),
    home_opens = sum(event_name == "Home_opened"),
    milestones_tracked = sum(event_name == "child_milestone_tracked"),
    time_used = difftime(max(event_timestamp),min(event_timestamp),units = 'days'),
    usage_count=length(event_timestamp),
    opened=ifelse(sum(grepl('_opened',event_name))>0,1,0))

serbia_att_users <- serbia%>%
  select(userid,endline)%>%
  group_by(userid)%>%
  summarise(endline_flag=sum(endline,na.rm=TRUE))%>% #ignoring follow up here
  filter(endline_flag==0)

bulgaria_att_users <- bulgaria%>%
  select(userid,endline)%>%
  group_by(userid)%>%
  summarise(endline_flag=sum(endline,na.rm=TRUE))%>% #ignoring follow up here
  filter(endline_flag==0)

att_users<-rbind(serbia_att_users,bulgaria_att_users)

app_usage<-app_usage%>%
  merge(att_users,by.x='facebook_id',by.y='userid',all.x=TRUE,all.y=FALSE)%>%
  mutate(attrition_flag=ifelse(is.na(endline_flag),0,1))
  
View(app_usage)


#############################################################
# App Usage Overview 
#############################################################

app_usage_weekly%>%
  merge(att_users,by.x='facebook_id',by.y='userid',all.x=TRUE,all.y=FALSE)%>%
  mutate(attrition_flag=ifelse(is.na(endline_flag),0,1))%>%
  dcast(facebook_id~week_since_start,value.var = "time_used")

# # 1. APP UPTAKE
# # Only participants assigned to treatment view the app, so we filter for treated participants to understand what percentage of treated downloaded the app and used it
# 
# serbia_home_opened <- serbia %>%
#   filter(treatment=='treated') %>%
#   mutate(app_downloaded = ifelse(opened,1,0),
#          home_opened_once = ifelse(home_opens>0,1,0),
#          home_opened_twice = ifelse(home_opens>1,1,0),
#          home_opened_thrice = ifelse(home_opens>3,1,0))%>%
#   summarise(total_treated=n(),
#             downloaded=sum(app_downloaded,na.rm=TRUE),
#             home_opened_once=sum(home_opened_once,na.rm=TRUE),
#             home_opened_twice=sum(home_opened_twice,na.rm=TRUE),
#             home_opened_thrice=sum(home_opened_thrice,na.rm=TRUE),
#             mean_usage_count=mean(usage_count,na.rm=TRUE),
#             mean_usage_time=mean(time_used,na.rm=TRUE))
# 
# bulgaria_home_opened <- bulgaria %>%
#   filter(treatment=='control') %>%
#   mutate(app_downloaded = ifelse(opened,1,0),
#          home_opened_once = ifelse(home_opens>0,1,0),
#          home_opened_twice = ifelse(home_opens>1,1,0),
#          home_opened_thrice = ifelse(home_opens>3,1,0))%>%
#   summarise(total_treated=n(),
#             downloaded=sum(app_downloaded,na.rm=TRUE),
#             home_opened_once=sum(home_opened_once,na.rm=TRUE),
#             home_opened_twice=sum(home_opened_twice,na.rm=TRUE),
#             home_opened_thrice=sum(home_opened_thrice,na.rm=TRUE),
#             mean_usage_count=mean(usage_count,na.rm=TRUE),
#             mean_usage_time=mean(time_used,na.rm=TRUE))
# 
# app_uptake<-as.data.table(rbind(serbia_home_opened,bulgaria_home_opened))
# app_uptake[,country:=c('Serbia','Bulgaria')]
