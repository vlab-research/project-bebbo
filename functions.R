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
