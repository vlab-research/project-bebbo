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
source("code/data.R")


#############################################################
# Number of Respondents
#############################################################

create_respondent_counts <- function(dat, col) {
    dat %>%
        group_by(country, .data[[col]]) %>%
        count() %>%
        pivot_wider(id_cols = {{ col }}, id_expand = TRUE, names_from = country, names_expand = FALSE, values_from = n) %>%
        ungroup() %>%
        mutate(
            variable = "survey stage",
            `Bulgaria %` = Bulgaria / sum(Bulgaria, rm.na = TRUE),
            `Serbia %` = Serbia / sum(Serbia, rm.na = TRUE)
        )
}



survey_stage <- dat %>%
    mutate(wave = recode(wave, `1` = "endline", `0` = "baseline", `2` = "followup")) %>%
    group_by(country, wave) %>%
    count() %>%
    pivot_wider(id_cols = wave, id_expand = TRUE, names_from = country, names_expand = FALSE, values_from = n) %>%
    ungroup() %>%
    mutate(
        variable = "survey stage",
        `Bulgaria %` = Bulgaria / sum(Bulgaria, rm.na = TRUE),
        `Serbia %` = Serbia / sum(Serbia, rm.na = TRUE)
    )


vars <- c("treatment", control_cols)
results <- lapply(vars, function(col) create_respondent_counts(dat %>% filter(wave == 0), col))

results <- c(list(survey_stage), results)
final <- rbindlist(results, use.names = FALSE)

final <- final %>%
    relocate(variable, .before = wave) %>%
    relocate(`Bulgaria %`, .before = Serbia)
names(final) <- c("variable", "value", "bulgaria", "bulgaria_prop", "serbia", "serbia_prop")
final <- final %>% mutate_if(is.numeric, round, 3)

final %>% write_table("report/descriptives/tables", "Baseline Respondent Characteristics")
