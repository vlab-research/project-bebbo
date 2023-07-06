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


#############################################################
# Variable creation
#############################################################


all_vars <- read_csv("data/raw/Bebbo Evaluation Survey - Pilot decisions - Updated baseline.csv") %>%
    filter(!is.na(Domain)) %>%
    pull(variable)

binary_confs <- list(
    bin_conf("breastfed", "Yes"),
    bin_conf("download_confirm_treatment", "Yes"),
    bin_conf("using_bebbo", "Yes")
)

knowledge_vars <- Filter(function(x) grepl("know", x), all_vars)
knowledge_confs <- knowledge_vars %>% lapply(function(x) bin_conf(x, "True"))

past_24_vars <- Filter(function(x) grepl("past_24h", x), all_vars)
past_24_confs <- past_24_vars %>% lapply(function(x) bin_conf(x, "Yes"))

outcomes <- c(knowledge_vars, past_24_vars, c("breastfed"))

binary_confs <- c(binary_confs, knowledge_confs, past_24_confs)


#############################################################
# Read Data
#############################################################

serbia <- read_csv("data/raw/serbia/responses.csv") %>%
    pick_serbia_resps() %>%
    mutate(seed_2 = seed %% 2 + 1) %>%
    mutate(
        treatment = recode(seed_2, `1` = "treated", `2` = "control"),
        endline = recode(shortcode, bebborsendeng = 1, bebborsbaseserb = 0)
    ) %>%
    binarize(binary_confs) %>%
    group_by(userid) %>%
    mutate(gender = first(parent_gender, order_by = endline, na_rm = TRUE)) %>%
    ungroup()

dat <- serbia
baseline <- dat %>% filter(endline == 0)
endline <- dat %>% filter(endline == 1)

## baseline %>%
##     group_by(treatment) %>%
##     summarize(across(all_of(outcomes), ~ mean(.x, na.rm = TRUE))) %>%
##     ungroup()

## bb <- baseline %>%
##     filter(!is.na(breastfed)) %>%

bb <- baseline


balance <- bal.tab(bb %>% select(c("gender", "breastfed", "past_24h_read")), bb$treatment, disp = c("means"))


balance_table <- balance$Balance %>%
    rename(control = M.0.Un, treatment = M.1.Un, difference = Diff.Un) %>%
    select(control, treatment, difference)

write_table(balance_table, "balance", summary = F)

write_table(balance$Observations, "balance-observations", summary = F)


models <- list(
    `Breastfed` = lm(breastfed ~ treatment * endline, dat),
    `Read` = lm(past_24h_read ~ treatment * endline, dat)
)
write_table(models, "regression", keep.stat = c("n"))
