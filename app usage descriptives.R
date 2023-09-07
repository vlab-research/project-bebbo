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
library(data.table)
library(ggplot2)
source("functions.R")
source("variable creation.R")

#############################################################
# Reading all datasets
#############################################################
# In this section, we read and perform some pre-processing for the datasets we will require for this analysis
# Currently, I don't filter for anything
# Possible filters to put in place - 
# ##### 1. Version
# ##### 2. Treatment/Control
# ##### 3. Endline/Baseline
# ##### 4. Stage of the survey reached by users
# ##### 5. shortcode based (intermediatebail or others)

# 1. Serbia Survey Respondents

serbia <- read_csv("data/raw/serbia/responses.csv") %>% #read Serbia responses file
  ind_endline(country='serbia')%>%
  likert(likert_confs) %>% #convert likert construct variables to likert scale
  binarize(binary_confs) %>% #binarize construct variables
  group_by(userid) %>%
  mutate(gender = first(parent_gender, order_by = endline, na_rm = TRUE)) %>%
  ungroup()#create variables to indicate endline
  

# 2. Bulgaria Survey Responses

bulgaria <- read_csv("data/raw/bulgaria/responses.csv") %>% #read Bulgaria responses file
  ind_endline(country='bulgaria')%>% #create variables to indicate endline
  likert(likert_confs) %>% #convert likert construct variables to likert scale
  binarize(binary_confs) %>% #binarize construct variables
  group_by(userid) %>%
  mutate(gender = first(parent_gender, order_by = endline, na_rm = TRUE)) %>%
  ungroup()

# 3. Creating construct variables

for (x in names(ss)) {
  serbia <- serbia %>%
    rowwise() %>%
    mutate("{x}" := mean(c_across(all_of(ss[[x]])),na.rm=TRUE))
  
  bulgaria <- bulgaria %>%
    rowwise() %>%
    mutate("{x}" := mean(c_across(all_of(ss[[x]])),na.rm=TRUE))
}

demo_cols<-c('parent_age','number_children','parent_gender','survey_duration','education')

serbia<-serbia%>%
  filter(endline==0)%>%
  mutate(parent_age=as.numeric(parent_age),
         number_children=as.numeric(number_children),
         age_flag=case_when(child_age=="0 to 6 months"~"0-2",
                            child_age=="6 to 12 months"~"0-2",
                            child_age=="12 to 24 months"~"0-2",
                            child_age=="2 to 4 years"~"2-6",
                            child_age=="4 to 6 years"~"2-6"),
         country = 'serbia')%>%
  select(c(userid,endline,demo_cols,age_flag,country,names(ss)))

bulgaria<-bulgaria%>%
  filter(endline==0)%>%
  mutate(parent_age=as.numeric(parent_age),
         number_children=as.numeric(number_children),
         age_flag=case_when(child_age=="0 to 6 months"~"0-2",
                            child_age=="6 to 12 months"~"0-2",
                            child_age=="12 to 24 months"~"0-2",
                            child_age=="2 to 4 years"~"2-6",
                            child_age=="4 to 6 years"~"2-6"),
         country = 'bulgaria')%>%
  select(c(userid,endline,demo_cols,age_flag,country,names(ss)))

constructs<-rbind(serbia,bulgaria)

# 4. Timing of events data
# We use this to find the time that users are asked to download the app

serbia_long<-read_csv("data/raw/serbia/serbia-long.csv")%>%
  filter(question_ref=='download_confirm_treatment')%>% #for each user, select the event details for when this question is asked
  select(userid,timestamp) #keep userid and timestamp of the event

bulgaria_long<-read_csv("data/raw/bulgaria/bulgaria-long.csv")%>%
  filter(question_ref=='download_confirm_treatment')%>%
  select(userid,timestamp)

download_time<-rbind(serbia_long,bulgaria_long)

app_usage<-read_csv("data/raw/bq-results-20230824.csv")

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

app_usage_weekly <- app_usage %>%
  merge(download_time,by.x='facebook_id',by.y='userid',all.x=TRUE,all.y=FALSE)%>%
  mutate(event_timestamp = parse_bq_date(event_timestamp), # event timestamp
         start_week = week(timestamp), # respondent's start week - week of respondent being asked to download the app
         event_day = wday(event_timestamp),
         week_since_start = round(difftime(event_timestamp,timestamp,units = "week"))) %>% # weeks since the start week (event timestamp - start time)
  group_by(facebook_id,week_since_start)%>% #respondent's activity by weeks since joining
  summarize(start_week=first(start_week),
            week_since_start=first(week_since_start),
            facebook_id = first(facebook_id),
            session_starts = sum(event_name == "session_start"),
            screen_views = sum(event_name == "screen_view"),
            user_engagements = sum(event_name == "user_engagement"), # user engagements 
            home_opens = sum(event_name == "Home_opened"), #total weekly home opens
            milestones_tracked = sum(event_name == "child_milestone_tracked"), 
            days_used = n_distinct(event_day), #hours used in the week
            usage_count=length(event_timestamp), #number of events logged in the week
            opened=ifelse(sum(grepl('_opened',event_name))>0,1,0), #any page opened binary
            home_opened=ifelse(sum(grepl('Home_opened',event_name))>0,1,0), #home page opened binary
            session_started=ifelse(sum(grepl('session_start',event_name))>0,1,0))%>% #session started binary
  mutate(week_since_start=week_since_start+1,
         week_after_treatment = paste0('week ',week_since_start))%>%
  filter(week_since_start>=1)

# 4b. App Usage Data weekly trends
weekly_aggregates<-app_usage_weekly%>%
  group_by(week_since_start)%>%
  summarise(respondents=n_distinct(facebook_id), #count of respondents
            opened=sum(opened), # count of respondents who opened the app
            home_opens=sum(home_opened), # count of respondents who opened the home page
            session_starts=sum(session_started), # count of respondents who started a session
            days_used=mean(days_used), #average number of hours used
            usage_count=mean(usage_count))%>% #number of events logged in the week
  mutate(week_after_treatment = paste0('week ',week_since_start))%>%
  filter(week_since_start>=1)%>%
  mutate_if(is.numeric,round,3)

weekly_aggregates%>%
  relocate(week_after_treatment,.before = respondents)%>%
  mutate(week_since_start=NULL)%>%
  write_table('weekly_aggregates')


#############################################################
# App Usage Over Time - PLOTS
#############################################################


ggplot(weekly_aggregates,aes(x=reorder(week_after_treatment,week_since_start),y=session_starts))+
  geom_bar(stat='identity',fill='lightblue')+
  geom_bar(aes(x=reorder(week_after_treatment,week_since_start),y=opened),stat='identity',position='identity',fill="#FF6C91",alpha=0.6)+
  geom_text(data=weekly_aggregates,aes(label=opened),size=3)+
  xlab('week after treatment')+
  ylab('number of respondents who started a session')

ggsave('app usage over time - respondents.png')

#event_count
ggplot(app_usage_weekly,aes(x=reorder(week_after_treatment,week_since_start),y=usage_count))+
  geom_boxplot(outlier.colour = 'pink',outlier.size = 0.7)+
  ylim(0,100)+
  geom_text(data=weekly_aggregates,aes(label=opened),size=3)+
  xlab('week after treatment')+
  ylab('# of events logged')

ggsave('app usage over time - usage count.png')

#days_used
ggplot(app_usage_weekly,aes(x=reorder(week_after_treatment,week_since_start),y=days_used))+
  geom_boxplot(outlier.colour = 'pink',outlier.size = 0.7)+
  ylim(0,6)+
  geom_text(data=weekly_aggregates,aes(label=opened),size=3)+
  xlab('week after treatment')+
  ylab('# of days used')

ggsave('app usage over time - days used.png')

#home opens
ggplot(app_usage_weekly,aes(x=reorder(week_after_treatment,week_since_start),y=home_opens))+
  geom_boxplot(outlier.colour = 'pink',outlier.size = 0.7)+
  ylim(0,10)+
  xlab('week after treatment')+
  ylab('# of home opens')

ggsave('app usage over time - home opens.png')

#############################################################
# Attrition, App Usage and Baseline Characteristics
#############################################################

# Attrition : I identify attrition by filtering for users that don't are present in baseline but don't reach the endline
# Attrition is a user level binary column that identifies respondents who didn't reach endline

# Baseline Characteristics : User level scores across the construct variables and demographic variables

# App Usage is a user level dataframe that combines respondents' app usage activity, baseline characteristics and attrition label
# We use this dataframe to answer the following questions : 
# 1. Do respondents' app usage predict their attrition?
# 2. Establish descriptives of baseline characteristics and app usage
# #### 2a. Note for later, we might have to filter the app usage activity for pre-endline activity. 
# ####     We can do so by filtering OUT week 14 since treatment and after

app_usage_user <- app_usage%>%
  merge(download_time,by.x='facebook_id',by.y='userid',all.x=TRUE,all.y=TRUE)%>%
  mutate(event_timestamp = parse_bq_date(event_timestamp),
         start_week = week(timestamp),
         event_day = wday(event_timestamp)) %>%
  group_by(facebook_id) %>% 
  summarise(
    start_week=first(start_week),
    timestamp=first(timestamp), #time asked to download the app
    userid = first(facebook_id),
    session_starts = sum(event_name == "session_start"),
    screen_views = sum(event_name == "screen_view"),
    user_engagements = sum(event_name == "user_engagement"),
    home_opens = sum(event_name == "Home_opened"),
    milestones_tracked = sum(event_name == "child_milestone_tracked"),
    days_used = n_distinct(event_day), #hours used in the week
    usage_count=length(event_name),
    opened=ifelse(sum(grepl('_opened',event_name))>0,1,0))%>%
  merge(constructs,by.x='facebook_id',by.y='userid',all.x=TRUE,all.y=TRUE) %>%
  mutate(downloaded=ifelse(is.na(session_starts),0,1), #if they have app data, they have downloaded the app
         treated = ifelse(is.na(timestamp),0,1))%>% #if they were asked to download the app, they have timestamp (from download_time dataset)
  filter(treated==1)

serbia_att_users <- serbia%>%
  select(userid,endline)%>%
  group_by(userid)%>%
  summarise(attrition_flag=sum(endline,na.rm=TRUE))%>% #ignoring follow up here
  filter(attrition_flag==0)

bulgaria_att_users <- bulgaria%>%
  select(userid,endline)%>%
  group_by(userid)%>%
  summarise(attrition_flag=sum(endline,na.rm=TRUE))%>% #ignoring follow up here
  filter(attrition_flag==0)

att_users<-rbind(serbia_att_users,bulgaria_att_users)

app_usage_user<-app_usage_user%>%
  merge(att_users,by.x='userid',by.y='userid',all.x=TRUE,all.y=FALSE)%>%
  mutate(attrition_flag=ifelse(is.na(attrition_flag),0,1))

#############################################################
# A. Baseline Characteristics and Downloads/Session Starts
#############################################################
# 
# Fit a linear regression model predicting the number of downloads/session starts given baseline characteristics
# Fit a logistic regression model predicting if the user downloads given baseline characteristics

#check for whether only run on downloaded
#this regression drops observations for which we don't have home opens i.e. the model fits only data for those who downloaded the app
#do baseline characteristics/demographics/survey patterns explain home opens?
lin_mod1<-lm('home_opens ~ start_week+
             dev_knw_recog+
             parent_knw+
             attitude+
             caregiver_well_being+
             self_care+
             family_care+
             practices_24+
             practices_agree+
             practices_hostility+
             parent_age+
             number_children+
             parent_gender+
             survey_duration+
             education+
             age_flag+
             country',
             data = app_usage_user)
lin_mod_home_opens<-summary(lin_mod1)
write_table(round(lin_mod_home_opens$coefficients,3),'app usage and baseline characteristics - linear reg')

log_mod1<-glm('downloaded ~ start_week+
             dev_knw_recog+
             parent_knw+
             attitude+
             caregiver_well_being+
             self_care+
             family_care+
             practices_24+
             practices_agree+
             practices_hostility+
             parent_age+
             number_children+
             parent_gender+
             survey_duration+
             education+
             age_flag+
             country',
             data = app_usage_user,
             family="binomial")
log_mod_downloaded<-summary(log_mod1)
write_table(round(log_mod_downloaded$coefficients,3),'downloads and baseline characteristics - logistic reg')
with(summary(log_mod1), 1 - deviance/null.deviance)




#############################################################
# B. Attrition and App Usage
#############################################################
#add a flag for calendar # of days used
colnames(app_usage)
mod1<-glm('attrition_flag ~ downloaded + session_starts + milestones_tracked + time_used + usage_count',data=app_usage_user,
          family=binomial())
summary(mod1)


