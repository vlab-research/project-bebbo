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
library(psych)
source("functions.R")
source("variable creation.R")


#############################################################
# Read Data
#############################################################

serbia <- read_csv("../data/raw/serbia/responses.csv") %>% #read Serbia responses file
  pick_serbia_resps(stage="end") %>% #filter dataframe for Serbia respondents
  ind_treatment_control() %>% #create variables to indicate treatment, control
  ind_endline(country='serbia') %>% #create variables to indicate endline
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

serbia<-serbia%>%mutate("merged_construct_1" := mean(c_across(all_of(c(ss[['parent_knw']],ss[['caregiver_well_being']]))),na.rm=TRUE))

#############################################################
# Reliability Analysis
#############################################################
serbia$merged_construct_1
alpha_matrix<-matrix(ncol=4,nrow = length(names(ss)))
alpha_drop_df<-data.frame()

serbia%>%filter(endline==0)%>%select(c(ss[['personal_nee']],ss[['parent_knw']],ss[['self_care']]))%>%psych::alpha()
serbia%>%filter(endline==0)%>%select(c('personal_needs','decrease_stress'))%>%psych::alpha()

for(att in c(names(ss),'merged_construct_1')){
  
  var_count<-length(ss[[att]])

  if (var_count>1){temp<-serbia%>%
    filter(endline==0)%>%
    select(all_of(ss[[att]]))%>%
    select_if(~ !all(is.na(.)))%>%
    alpha()
  
  raw_alpha<-round(temp$total[1]$raw_alpha,2)
  std_alpha<-round(temp$total[2]$std.alpha,2)
  
  alpha_drop<-temp$alpha.drop[,c('raw_alpha','std.alpha')]%>%round(2)
  alpha_drop$construct<-att
  
  alpha_drop_df<-rbind(alpha_drop_df,alpha_drop)
  }
  else{
    raw_alpha<-NA
    std_alpha<-NA
  }
  
  ind<-which(names(ss)==att)
  alpha_matrix[ind,]<- c(att,var_count,raw_alpha,std_alpha)
}


colnames(alpha_matrix)<-c("construct","variable count","raw.alpha","std.alpha")
write_table(alpha_matrix,'constructs_alpha_matrix',align=TRUE)
alpha_matrix<-as.data.frame(alpha_matrix)

alpha_drop_df$variable.dropped<-rownames(alpha_drop_df)
rownames(alpha_drop_df)<-NULL

alpha_drop_df<-alpha_drop_df%>%
  merge(alpha_matrix[,c('construct','raw.alpha')],by.x='construct',by.y='construct')%>%
  mutate(raw.alpha=as.numeric(raw.alpha))%>%
  mutate(increment=raw_alpha-raw.alpha)%>%
  mutate(raw.alpha=NULL)%>%
  relocate(c(variable.dropped,increment),.after=construct)

write_table(alpha_drop_df,'alpha_drop',align=TRUE)



dat <- serbia%>%filter(endline==0)%>%select(all_of(unname(unlist(ss))))%>%select_if(~ !any(is.na(.)))
pca_dat <- prcomp(dat,scale = TRUE,center = TRUE)
plot(c(1:length(pca_dat$sdev)),(pca_dat$sdev^2)/sum(pca_dat$sdev^2))
loadings <- as.data.frame(pca_dat$rotation[,c(1:7)])%>%round(2)
loadings%>%as.matrix()%>%corrplot(method='color',tl.cex = 0.75,cl.cex = 0.75)

write_table(loadings,'all_variables_loadings',align=TRUE)


# dat <- serbia%>%filter(endline==0)%>%select(all_of(ss[['dev_knw_concern_0_2']]))%>%drop_na()
# pca_dat <- prcomp(dat,scale = TRUE,center = TRUE)
# plot(c(1:length(pca_dat$sdev)),(pca_dat$sdev^2)/sum(pca_dat$sdev^2))
# loadings <- as.data.frame(pca_dat$rotation[,c(1:4)])
# 
# dat <- serbia%>%filter(endline==0)%>%select(all_of(ss[['dev_knw_concern_3_6']]))%>%drop_na()
# pca_dat <- prcomp(dat,scale = TRUE,center = TRUE)
# plot(c(1:length(pca_dat$sdev)),(pca_dat$sdev^2)/sum(pca_dat$sdev^2))
# loadings <- as.data.frame(pca_dat$rotation[,c(1:4)])

# dat <- serbia%>%filter(endline==0)%>%select(all_of(ss[['practices_24']]))%>%drop_na()
# pca_dat <- prcomp(dat,scale = TRUE,center = TRUE)
# plot(c(1:length(pca_dat$sdev)),(pca_dat$sdev^2)/sum(pca_dat$sdev^2))
# loadings <- as.data.frame(pca_dat$rotation[,c(1:4)])

# dat <- serbia%>%filter(endline==0)%>%select(all_of(ss[['dev_knw_recog']]))%>%drop_na()
# pca_dat <- prcomp(dat,scale = TRUE,center = TRUE)
# plot(c(1:length(pca_dat$sdev)),(pca_dat$sdev^2)/sum(pca_dat$sdev^2))
# loadings <- as.data.frame(pca_dat$rotation[,c(1:4)])
