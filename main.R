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



#############################################################
# Variable creation
#############################################################
key <- read_csv("data/raw/Bebbo Evaluation Survey - Serbia - English Analysis notes - Baseline.csv")

all_vars <- key %>%
    filter(!is.na(Domain)) %>%
    pull(variable)

binary_confs <- list(
    bin_conf("download_confirm_treatment", "Yes"),
    bin_conf("using_bebbo", "Yes")
)

binarized_vars <- key %>%
    filter(!is.na(construct_variable)) %>%
    rowwise() %>%
    mutate(foo = list(bin_conf(variable, Correct))) %>%
    pull(foo)

binary_confs <- c(binary_confs, binarized_vars)


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


s <- key %>%
    filter(!is.na(construct_variable)) %>%
    pull(construct_variable) %>%
    unique()

ss <- sapply(s, function(x) {
    key %>%
        filter(construct_variable == x) %>%
        pull(variable)
})

for (x in names(ss)) {
    serbia <- serbia %>%
        rowwise() %>%
        mutate("{x}" := mean(c_across(all_of(ss[[x]]))))
}


## bulgaria <- read_csv("data/raw/bulgaria/responses.csv") %>%
##     mutate(country = "bulgaria")

dat <- serbia
baseline <- dat %>% filter(endline == 0)
endline <- dat %>% filter(endline == 1)


bb <- baseline


balance <- bal.tab(bb %>% select(c("gender", "breastfed", "past_24h_read")), bb$treatment, disp = c("means"))


balance_table <- balance$Balance %>%
    rename(control = M.0.Un, treatment = M.1.Un, difference = Diff.Un) %>%
    select(control, treatment, difference)

write_table(balance_table, "balance", summary = F)

write_table(balance$Observations, "balance-observations", summary = F)


models <- list(
    `Child Development Knowledge Recognition` = lm(dev_knw_recog ~ treatment * endline, dat),
    `Child Development Knowledge Concern 0-2` = lm(dev_knw_concern_0_2 ~ treatment * endline, dat),
    `Health Knowledge` = lm(health_knw ~ treatment * endline, dat),
    `Practice Past 24 Hours` = lm(practices_24 ~ treatment * endline, dat),
    `Breastfed` = lm(was_breastfed ~ treatment * endline, dat)
)

write_table(models, "regression", keep.stat = c("n"), float.env = "sidewaystable")
