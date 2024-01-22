library(readr)

library(stringr)
library(dplyr)
library(purrr)
library(tidyr)

source("code/functions.R")
source("code/app_data.R")

#############################################################
# Variable creation
#############################################################

key <- read_csv("data/raw/Bebbo Evaluation Survey - Serbia - English Analysis notes.xlsx - Baseline.csv")

key[which(key$variable == "past_24h_play"), "Correct"] <- "Yes"

all_vars <- key %>%
    filter(!is.na(Domain)) %>%
    pull(variable)

# create a map of correct responses for two variables
binary_confs <- list(
    bin_conf("download_confirm_treatment", "Yes"),
    bin_conf("using_bebbo", "Yes"),
    bin_conf("education", c("University"), "university"),
    bin_conf("parent_gender", c("Woman"), "woman"),
    bin_conf("language", c("Serbian", "Bulgarian"), "dominant_language"),
    bin_conf("relationship_to_child", c("Mother", "Father"), "is_parent"),
    bin_conf("location", c("Capital", "Large regional centers"), "urban")
)

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
    filter(!is.na(Correct)) %>%
    rowwise() %>%
    mutate(foo = list(bin_conf(variable, Correct))) %>%
    pull(foo)

likert_confs <- key %>%
    filter(variable %in% likert_cols) %>%
    rowwise() %>%
    mutate(foo = list(likert_conf(variable, Correct, answers))) %>%
    pull(foo)

# final map
binary_confs <- c(binary_confs, binarized_vars)

# constructs
## construct_cols <- key %>%
##     filter(!is.na(construct_variable)) %>%
##     pull(construct_variable) %>%
##     unique()


domains <- list(
    `Knowledge and Awareness` = c("health_knw", "dev_knw_recog"),
    `Confidence and Attitudes` = c("confidence", "attitude"),
    `Practices` = c("was_breastfed", "practices_24", "practices_agree", "practices_hostility")
)

construct_cols <- as.character(flatten(domains))

# grab all variables associated with the constructs in s
ss <- sapply(construct_cols, function(x) {
    key %>%
        filter(construct_variable == x) %>%
        pull(variable)
})


variable_cols <- unname(unlist(ss)) # all variables from constructs
binary_cols <- sapply(binarized_vars, function(x) x$col)
baseline_control_cols <- key %>%
    filter(`Baseline Control` == TRUE) %>%
    pull(variable)

domains_constructs_variables <- key %>%
    filter(!is.na(construct_variable)) %>%
    select(Domain, construct_variable, variable)

control_cols <- c(
    "woman",
    "university",
    "dominant_language",
    "is_parent",
    "age_flag",
    "children_count",
    "parent_age_flag",
    "urban"
)


add_app_usage_features <- function(dat) {
    start_times <- dat %>%
        select(userid, baseline_start, endline_start, followup_start) %>%
        group_by(userid) %>%
        summarise_all(first)

    f <- app_events %>%
        inner_join(start_times, by = "userid") %>%
        mutate(event_wave = case_when(
            (event_timestamp >= baseline_start) & (event_timestamp <= endline_start) ~ 1,
            (event_timestamp >= endline_start) & (event_timestamp <= followup_start) ~ 2,
            TRUE ~ NA
        )) %>%
        filter(!is.na(event_wave)) %>%
        rollup_events(c("event_wave")) %>%
        rename(wave = event_wave)

    wave_2 <- f %>%
        filter(!is.na(wave)) %>%
        group_by(userid) %>%
        summarise(across(where(is.numeric), ~ sum(.x)), across(where(is.logical), ~ any(.x))) %>%
        ungroup() %>%
        mutate(wave = 2)

    wave_1 <- f %>% filter(wave == 1)

    waves <- rbind(wave_1, wave_2)

    dat %>%
        left_join(waves, by = c("userid", "wave")) %>%
        mutate(has_learning_event = if_else(is.na(has_learning_event), FALSE, has_learning_event))
}


additional_features <- function(dat) {
    dat %>%
        mutate(
            parent_age = as.numeric(parent_age),
            number_children = as.numeric(number_children),
            age_flag = case_when(
                child_age == "0 to 6 months" ~ "0-2",
                child_age == "6 to 12 months" ~ "0-2",
                child_age == "12 to 24 months" ~ "0-2",
                child_age == "2 to 4 years" ~ "2-6",
                child_age == "4 to 6 years" ~ "2-6"
            ),
            children_count = case_when(
                number_children < 4 ~ "1-3",
                number_children >= 4 ~ "4+"
            ),
            parent_age_flag = case_when(
                parent_age <= 35 ~ "35 and under",
                parent_age > 35 ~ "Over 35"
            ),
        ) %>%
        add_app_usage_features()
}



add_start_times <- function(dat) {
    base <- dat %>%
        filter(wave == 0) %>%
        select(userid, survey_start_time) %>%
        rename(baseline_start = survey_start_time)

    end <- dat %>%
        filter(wave == 1) %>%
        select(userid, survey_start_time) %>%
        rename(endline_start = survey_start_time)

    follow <- dat %>%
        filter(wave == 2) %>%
        select(userid, survey_start_time) %>%
        rename(followup_start = survey_start_time)

    starts <- base %>%
        left_join(end, by = "userid") %>%
        left_join(follow, by = "userid")

    dat %>% left_join(starts, by = "userid")
}


add_controls <- function(dat) {
    controls <- dat %>%
        filter(wave == 0) %>%
        select(userid, baseline_control_cols)

    dat %>%
        select(-baseline_control_cols) %>%
        inner_join(controls, by = "userid")
}

normalize_variables <- function(dat) {
    dat %>%
        add_controls() %>%
        add_start_times() %>%
        likert(likert_confs) %>% # convert likert construct variables to likert scale
        binarize(binary_confs) %>% # binarize construct variables
        group_by(userid) %>%
        mutate(
            parent_age = as.numeric(parent_age),
            parent_age = ifelse(parent_age > 100, NA, parent_age)
        ) %>%
        ungroup() %>%
        additional_features()
}


clean_responses <- function(dat) {
    dat %>%
        filter(!is.na(parent_age_flag)) %>%
        filter(answer_time_90 / answer_time_min >= 1.5)
}

make_translation_lookup <- function(long) {
    long %>%
        group_by(shortcode, surveyid, question_ref) %>%
        filter(!is.na(response) & !is.na(translated_response)) %>%
        slice(1) %>%
        select(shortcode, surveyid, question_ref, response, translated_response)
}


#############################################################
# Data sets for analysis
#############################################################

# 1. Serbia Survey Respondents
serbia <- read_csv("data/raw/serbia/responses.csv") %>% # read Serbia responses file
    pick_serbia_resps() %>% # filter dataframe for Serbia respondents
    ind_treatment_control("1") %>% # create variables to indicate treatment, control
    ind_endline(country = "serbia") %>% # create variables to indicate endline
    normalize_variables() %>%
    clean_responses()


# 2. Bulgaria Survey Responses
flipped_cols <- c(
    "laugh_together",
    "joke_with_child",
    "smile_around_child",
    "play_on_floor",
    "snap_at_child",
    "lose_patience_punish",
    "threaten",
    "make_fun_of"
)

bulgaria <- read_csv("data/raw/bulgaria/responses.csv") %>% # read Bulgaria responses file
    pick_bulgaria_resps() %>% # filter dataframe for Bulgarian respondents
    ind_treatment_control("2") %>% # create variables to indicate treatment, control
    ind_endline(country = "bulgaria") %>% # create variables to indicate endline
    normalize_variables() %>%
    clean_responses() %>%
    flip_likerts(flipped_cols)


for (x in names(ss)) {
    serbia <- serbia %>%
        rowwise() %>%
        mutate("{x}" := mean(c_across(all_of(ss[[x]])), na.rm = TRUE)) %>%
        ungroup()

    bulgaria <- bulgaria %>%
        rowwise() %>%
        mutate("{x}" := mean(c_across(all_of(ss[[x]])), na.rm = TRUE)) %>%
        ungroup()
}

# Quick hack to fix practices_24 to be sum, not mean
bulgaria <- bulgaria %>%
    rowwise() %>%
    mutate("practices_24" := sum(c_across(all_of(ss[["practices_24"]])), na.rm = FALSE)) %>%
    ungroup()

serbia <- serbia %>%
    rowwise() %>%
    mutate("practices_24" := sum(c_across(all_of(ss[["practices_24"]])), na.rm = FALSE)) %>%
    ungroup()

serbia$form_test <- NULL

bulgaria_with_impacted <- bulgaria
serbia_with_impacted <- serbia

bulgaria <- bulgaria %>% filter(!impacted)
serbia <- serbia %>% filter(!impacted)

serbia <- serbia %>% relocate(colnames(bulgaria))
dat <- rbind(serbia, bulgaria)
pooled <- rbind(serbia_with_impacted, bulgaria_with_impacted)
pooled_without_impacted <- rbin(serbia, bulgaria)
