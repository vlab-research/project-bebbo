library(tidyr)
source("code/data.R")


datasets <- list(
    `Serbia` = serbia,
    `Bulgaria` = bulgaria_with_impacted,
    `Pooled` = pooled
)

out <- tibble()

for (dataset in names(datasets)) {
    dat <- datasets[[dataset]]

    dat <- dat %>% 
        filter(wave == 1) %>% 
        filter(treatment == "treated")

    d <- dat %>% 
        summarise(
        treated = length(userid), 
        has_learned = length(userid[has_learning_event]), 
        learned_more_than_one_day = length(userid[!is.na(days_learned) & days_learned > 1]),
        learned_more_than_three_days = length(userid[!is.na(days_learned) & days_learned > 3]) )%>%
    mutate(has_learned_perc = has_learned / treated, 
           more_than_one_day_perc = learned_more_than_one_day / treated,
           more_than_three_days_perc = learned_more_than_three_days / treated
           ) %>% 
    rename(
        `Treated` = treated, 
        `Used App` = has_learned, 
        `Used App Ratio` = has_learned_perc, 
        `Used App More Than One Day` = learned_more_than_one_day, 
        `Used App More Than One Day Ratio` = more_than_one_day_perc,
        `Used App More Than Three Days Ratio` = more_than_three_days_perc
    )

    out <- rbind(out, d)
}

write_table(out, "report/descriptives", "Treatment Takeup.tex")

