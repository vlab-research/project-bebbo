library(readr)
library(dplyr)

source("code/functions.R")

#############################################################
# Variable creation
#############################################################

key <- read_csv("data/raw/Bebbo Evaluation Survey - Serbia - English Analysis notes.xlsx - Baseline.csv")
## key <- read_csv("data/raw/key baseline 09-19.csv")

key[which(key$variable == "past_24h_play"), "Correct"] <- "Yes"

all_vars <- key %>%
    filter(!is.na(Domain)) %>%
    pull(variable)

# create a map of correct responses for two variables
binary_confs <- list(
    bin_conf("download_confirm_treatment", "Yes"),
    bin_conf("using_bebbo", "Yes")
)

# TODO: redo this to include controls, dropped variables, and constructs
# all from the excel...

# create a map of correct responses for each construct variable
likert_cols <- key %>%
    select(c("variable", "Correct")) %>%
    rowwise() %>%
    mutate(check = length(parse_mult_choice(Correct))) %>%
    ungroup() %>%
    filter(check > 1) %>%
    pull(variable)

binarized_vars <- key %>%
    filter(!variable %in% likert_cols) %>%
    filter(!is.na(construct_variable)) %>%
    rowwise() %>%
    mutate(foo = list(bin_conf(variable, Correct))) %>%
    pull(foo)

likert_confs <- key %>%
    filter(variable %in% likert_cols) %>%
    filter(!is.na(construct_variable)) %>%
    rowwise() %>%
    mutate(foo = list(likert_conf(variable, Correct, answers))) %>%
    pull(foo)

# final map
binary_confs <- c(binary_confs, binarized_vars)

# constructs
s <- key %>%
    filter(!is.na(construct_variable)) %>%
    pull(construct_variable) %>%
    unique()

# grab all variables associated with the constructs in s
ss <- sapply(s, function(x) {
    key %>%
        filter(construct_variable == x) %>%
        pull(variable)
})

variable_cols <- unname(unlist(ss)) # all variables
construct_cols <- s # all constructs
#############################################################
# Data sets for analysis
#############################################################

# 1. Serbia Survey Respondents
serbia <- read_csv("data/raw/serbia/responses.csv") %>% # read Serbia responses file
    pick_serbia_resps() %>% # filter dataframe for Serbia respondents
    ind_treatment_control() %>% # create variables to indicate treatment, control
    ind_endline(country = "serbia") %>% # create variables to indicate endline
    likert(likert_confs) %>% # convert likert construct variables to likert scale
    binarize(binary_confs) %>% # binarize construct variables
    group_by(userid) %>%
    mutate(gender = first(parent_gender, order_by = endline, na_rm = TRUE)) %>%
    ungroup()


# 2. Bulgaria Survey Responses
bulgaria <- read_csv("data/raw/bulgaria/responses.csv") %>% # read Bulgaria responses file
    pick_bulgaria_resps() %>% # filter dataframe for Bulgarian respondents
    ind_treatment_control() %>% # create variables to indicate treatment, control
    ind_endline(country = "bulgaria") %>% # create variables to indicate endline
    likert(likert_confs) %>% # convert likert construct variables to likert scale
    binarize(binary_confs) %>% # binarize construct variables
    group_by(userid) %>%
    mutate(gender = first(parent_gender, order_by = endline, na_rm = TRUE)) %>%
    ungroup()


for (x in names(ss)) {
    serbia <- serbia %>%
        rowwise() %>%
        mutate("{x}" := mean(c_across(all_of(ss[[x]])), na.rm = TRUE))

    bulgaria <- bulgaria %>%
        rowwise() %>%
        mutate("{x}" := mean(c_across(all_of(ss[[x]])), na.rm = TRUE))
}

# Quick hack to fix practices_24 to be sum, not mean
bulgaria <- bulgaria %>%
    rowwise() %>%
    mutate("practices_24" := sum(c_across(all_of(ss[["practices_24"]])), na.rm = TRUE))

serbia <- serbia %>%
    rowwise() %>%
    mutate("practices_24" := sum(c_across(all_of(ss[["practices_24"]])), na.rm = TRUE))
