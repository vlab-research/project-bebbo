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
    x,
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
  
  new_col <- paste0(col,'_likert')
  
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
  
  df<-mutate(df, "{new_col}":=case_when(targ %in% c('C','D','E') ~ recode(col,
                                                                     a_matched="1",
                                                                     b_matched="2",
                                                                     c_matched="3",
                                                                     d_matched="4",
                                                                     e_matched="5"),
                                    targ %in% c('A','B') ~ recode(col,
                                                                  a_matched="5",
                                                                  b_matched="4",
                                                                  c_matched="3",
                                                                  d_matched="2",
                                                                  e_matched="1")))
}

likert <- function(df, cols) {
  likert_cols<-c('parenting_stress_1','parenting_stress_2')
  for (c in cols) {
    if (c$col %in% likert_cols) {
      df <- likert_col(df, c$col, c$target, c$answers)
    }
  }
  df
}
