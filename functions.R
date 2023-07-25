library(readr)
library(dplyr)
library(cobalt)
library(glue)
library(stargazer)


binarize_col <- function(df, col, targ, new_col = NULL) {
  if (is.null(new_col)) {
    new_col <- col
  }
  c <- df[[col]]
  
  s <- as.integer(dplyr::case_when(
    is.na(c) ~ NA,
    c %in% targ ~ TRUE,
    TRUE ~ FALSE
  ))
  
  dplyr::mutate(df, "{new_col}" := s)
}


bin_conf <- function(col, target, new_col = NULL) {
  list(col = col, target = target, new_col = new_col)
}

likert_conf <- function(col, target, answers) {
  list(col = col, target = target, answers = answers)
}

binarize <- function(df, cols) {
  for (c in cols) {
    if (c$col %in% colnames(df)) {
      df <- binarize_col(df, c$col, c$target, c$new_col)
    }
  }
  df
}

write_table <- function(x, filename, ...) {
  stargazer(
    as.matrix(x),
    out = glue("tables/{filename}.tex"),
    digits = 2,
    label = glue("tbl:{filename}"),
    ...
  )
}

pick_serbia_resps <- function(df) {
  users <- df %>%
    filter(shortcode == "bebborsendeng") %>%
    filter(thankyou_you_qualify == "OK") %>%
    filter(version > 4) %>%
    distinct(userid) %>%
    pull(userid)
  
  
  df %>%
    filter(shortcode != "bebborsintermediatebail") %>%
    filter(userid %in% users)
}


pick_bulgaria_resps <- function(df) {
  users <- df %>%
    filter(shortcode == "bebbobgendeng") %>%
    filter(thankyou_you_qualify == "OK") %>%
    ## filter(version > 4) %>% # TODO: get bulgaria version
    distinct(userid) %>%
    pull(userid)
  
  df %>%
    filter(userid %in% users)
}

ind_treatment_control <- function(df) {
  df %>% 
    mutate(seed_2 = seed %% 2 + 1) %>% #create seed_2
    mutate(treatment = recode(seed_2, `1` = "treated", `2` = "control")) #create treatment variable based on seed_2
}

ind_endline <- function(df) {
  df %>% mutate(endline = recode(shortcode, bebborsendeng = 1, bebborsbaseserb = 0))#create end line variable based on short code
}


parse_mult_choice<-function(x) {
  temp<-match(unlist(strsplit(x,split=', ')),c('A','B','C','D','E'))
  c('A','B','C','D','E')[temp]
}

likert_col <- function(df, col, targ, ans) {
  
  # new_col <- paste0(col,'_likert')
  new_col <- col
  
  a_pattern="-A. ([A-Za-z ]+)\n"
  b_pattern="-B. ([A-Za-z ]+)\n"
  c_pattern="-C. ([A-Za-z ]+)\n"
  d_pattern="-D. ([A-Za-z ]+)"
  e_pattern="-E. ([A-Za-z ]+)"
  
  a_matched=str_extract(ans,pattern = a_pattern,group=1)
  b_matched=str_extract(ans,pattern = b_pattern,group=1)
  c_matched=str_extract(ans,pattern = c_pattern,group=1)
  d_matched=str_extract(ans,pattern = d_pattern,group=1)
  e_matched=str_extract(ans,pattern = e_pattern,group=1)
  
  targ<-parse_mult_choice(targ)[1]
  
  likert_asc<-recode(df[[col]],"{a_matched}":=1,"{b_matched}":=2,"{c_matched}":=3,"{d_matched}":=4,"{e_matched}":=5)
  likert_desc<-recode(df[[col]],"{a_matched}":=5,"{b_matched}":=4,"{c_matched}":=3,"{d_matched}":=2,"{e_matched}":=1)
  
  df%>%mutate("{new_col}" := case_when(targ %in% c('C','D','E') ~ likert_asc, targ %in% c('A','B') ~ likert_desc))
}

likert <- function(df, cols) {
  for (c in cols) {
    if (c$col %in% colnames(df)) {
      df <- likert_col(df, c$col, c$target, c$answers)
    }
  }
  df
}

descriptives_prop<-function(df,cols) {
  df %>% summarise(prop_0=sum(case_when(value==0~1,value==1~0,TRUE~NA),na.rm = TRUE)/length(value),
                   prop_1=sum(case_when(value==1~1,is.na(value)~NA, TRUE~0),na.rm = TRUE)/length(value),
                   prop_2=sum(case_when(value==2~1,value==0~NA, is.na(value)~NA, TRUE~0),na.rm = TRUE)/length(value),
                   prop_3=sum(case_when(value==3~1,value==0~NA, is.na(value)~NA, TRUE~0),na.rm = TRUE)/length(value),
                   prop_4=sum(case_when(value==4~1,value==0~NA, is.na(value)~NA, TRUE~0),na.rm = TRUE)/length(value),
                   prop_5=sum(case_when(value==5~1,value==0~NA, is.na(value)~NA, TRUE~0),na.rm = TRUE)/length(value),
                   mean=mean(value,na.rm=TRUE),
                   sd=sd(value,na.rm=TRUE),
                   prop_na=mean(is.na(value))
                )
  
}

descriptives_summary<-function(df,cols) {
  df %>% summarise(mean=mean(value,na.rm=TRUE),
                   median=median(value,na.rm=TRUE),
                   min=min(value,na.rm=TRUE),
                   max=max(value,na.rm=TRUE),
                   sd=sd(value,na.rm=TRUE),
                   prop_na=mean(is.na(value))
  )
  
}