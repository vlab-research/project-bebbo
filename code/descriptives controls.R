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
library(lattice)
library(nFactors)
library(lavaan)
library(moments)
library(data.table)
source("code/functions.R")
source("code/variable creation.R")

#############################################################
# Read Data
#############################################################
# In this section, we read and perform some pre-processing for the datasets we will require for this analysis

# code<-"bebborsbaseserb" #filter for users who reach baseline
# code<-"bebborsendeng" #filter for users who reach endline
# code<-"bebborsfueng" #filter for users who reach followup

# 1. Serbia Survey Respondents
serbia <- read_csv("data/raw/serbia/responses.csv") %>% # read Serbia responses file
    # pick_serbia_resps() %>% #filter dataframe for Serbia respondents
    ind_treatment_control() %>% # create variables to indicate treatment, control
    ind_endline(country = "serbia") %>% # create variables to indicate endline
    likert(likert_confs) %>% # convert likert construct variables to likert scale
    binarize(binary_confs) %>% # binarize construct variables
    group_by(userid) %>%
    mutate(
        gender = first(parent_gender, order_by = endline, na_rm = TRUE),
        country = "serbia"
    ) %>%
    ungroup()


# code<-"bebbobg2basebul" #filter for users who reach baseline
# code<-"bebbobgendeng" #filter for users who reach endline
# code<-"bebbobgfueng" #filter for users who reach followup

# 2. Bulgaria Survey Responses
bulgaria <- read_csv("data/raw/bulgaria/responses.csv") %>% # read Bulgaria responses file
    # pick_bulgaria_resps(code="bebbobg2basebul") %>% #filter dataframe for Bulgarian respondents
    ind_treatment_control() %>% # create variables to indicate treatment, control
    ind_endline(country = "bulgaria") %>% # create variables to indicate endline
    likert(likert_confs) %>% # convert likert construct variables to likert scale
    binarize(binary_confs) %>% # binarize construct variables
    group_by(userid) %>%
    mutate(
        gender = first(parent_gender, order_by = endline, na_rm = TRUE),
        country = "bulgaria"
    ) %>%
    ungroup()

#############################################################
# Number of Respondents
#############################################################

serbia$form_test <- NULL
serbia <- serbia %>% relocate(colnames(bulgaria))
dat <- rbind(serbia, bulgaria)
dat <- dat %>% mutate(
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
    education = recode(education, University = 1, Basic = 0, `Incomplete basic` = 0, Secondary = 0)
)

stat1 <- dat %>%
    count(country, treatment) %>%
    pivot_wider(id_cols = treatment, id_expand = TRUE, names_from = country, names_expand = FALSE, values_from = n) %>%
    mutate(variable = "treatment")

stat2 <- dat %>%
    mutate(endline = recode(endline, `1` = "endline", `0` = "baseline", `2` = "followup")) %>%
    group_by(country, endline) %>%
    count() %>%
    pivot_wider(id_cols = endline, id_expand = TRUE, names_from = country, names_expand = FALSE, values_from = n) %>%
    mutate(variable = "survey stage")

stat3 <- dat %>%
    group_by(country, age_flag) %>%
    count() %>%
    pivot_wider(id_cols = age_flag, id_expand = TRUE, names_from = country, names_expand = FALSE, values_from = n) %>%
    mutate(variable = "age_flag")

stat4 <- dat %>%
    mutate(education = recode(endline, `1` = "University educated", `0` = "Not University educated")) %>%
    group_by(country, education) %>%
    count() %>%
    pivot_wider(id_cols = education, id_expand = TRUE, names_from = country, names_expand = FALSE, values_from = n) %>%
    mutate(variable = "education")

stat5 <- dat %>%
    group_by(country, children_count) %>%
    count() %>%
    pivot_wider(id_cols = children_count, id_expand = TRUE, names_from = country, names_expand = FALSE, values_from = n) %>%
    mutate(variable = "children_count")

stat6 <- dat %>%
    group_by(country, parent_age_flag) %>%
    count() %>%
    pivot_wider(id_cols = parent_age_flag, id_expand = TRUE, names_from = country, names_expand = FALSE, values_from = n) %>%
    mutate(variable = "parent age")

final <- rbindlist(list(stat1, stat2, stat3, stat4, stat5), use.names = FALSE)
final <- final %>% relocate(variable, .before = treatment)
names(final) <- c("variable", "value", "bulgaria", "serbia")
final <- final %>%
    mutate(
        bulgaria_prop = bulgaria / table(dat$country)[1],
        serbia_prop = serbia / table(dat$country)[2]
    ) %>%
    mutate_if(is.numeric, round, 3) %>%
    mutate(value = ifelse(is.na(value), "NA", value))

final %>% write_table("descriptives_controls_respondent_count")
