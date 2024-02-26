library(tidyr)
source("code/data.R")


datasets <- list(
    `Serbia` = serbia,
    `Bulgaria` = bulgaria,
    `Pooled` = pooled
)


##########################################
# Takeup Table
#########################################

out <- tibble()

for (dataset in names(datasets)) {
    dat <- datasets[[dataset]]

    dat <- dat %>%
        filter(wave == 1) %>%
        filter(treatment == "treated")

    d <- dat %>%
        summarise(
            treated = length(userid),
            has_downloaded = length(userid[has_any_bebbo_event]),
            has_learned = length(userid[has_learning_event]),
            learned_more_than_one_day = length(userid[!is.na(days_learned) & days_learned > 1]),
            learned_more_than_three_days = length(userid[!is.na(days_learned) & days_learned > 3])
        ) %>%
        mutate(
            dataset = dataset,
            has_learned_perc = has_learned / treated,
            more_than_one_day_perc = learned_more_than_one_day / treated,
            more_than_three_days_perc = learned_more_than_three_days / treated
        ) %>%
        rename(
            `Dataset` = dataset,
            `Treated` = treated,
            `Downloaded` = has_downloaded,
            `Used` = has_learned,
            `Used (%)` = has_learned_perc,
            `> 1 Day` = learned_more_than_one_day,
            `> 1 Day (%)` = more_than_one_day_perc,
            `> 3 Days` = learned_more_than_three_days,
            `> 3 Days (%)` = more_than_three_days_perc
        ) %>%
        relocate(Dataset, .before = Treated) %>%
        mutate_if(is.numeric, round, 3)

    out <- rbind(out, d)
}

write_table(out, "report/descriptives", "Treatment Takeup", table.placement = "H")


##########################################
# APP RETENTION FUNNEL
#########################################

out <- tibble()

for (dataset in names(datasets)) {
    dat <- datasets[[dataset]]

    dat <- dat %>%
        filter(wave == 1) %>%
        filter(treatment == "treated")

    d <- dat %>%
        summarise(
            has_downloaded = length(userid[has_any_bebbo_event]),
            has_learned = length(userid[has_learning_event]),
            learned_more_than_one_day = length(userid[!is.na(days_learned) & days_learned > 1]),
            learned_more_than_three_days = length(userid[!is.na(days_learned) & days_learned > 3])
        ) %>%
        mutate(
            dataset = dataset,
            has_learned_perc = has_learned / has_downloaded,
            more_than_one_day_perc = learned_more_than_one_day / has_downloaded,
            more_than_three_days_perc = learned_more_than_three_days / has_downloaded
        ) %>%
        rename(
            `Dataset` = dataset,
            `Downloaded` = has_downloaded,
            `Used (%)` = has_learned_perc,
            `> 1 Day (%)` = more_than_one_day_perc,
            `> 3 Days (%)` = more_than_three_days_perc
        ) %>%
        relocate(Dataset, .before = Downloaded) %>%
        mutate_if(is.numeric, round, 3) %>%
        select(-has_learned, -learned_more_than_one_day, -learned_more_than_three_days)

    out <- rbind(out, d)
}

write_table(out, "report/descriptives", "App Retention Funnel", table.placement = "H")

##########################################
# Days with Learning Events Histogram
#########################################

pooled %>%
    filter(wave == 1) %>%
    filter(treatment == "treated") %>%
    select(userid, country, days_learned) %>%
    ggplot(aes(x = days_learned)) +
    stat_bin(binwidth = 1, fill = "steelblue") +
    stat_bin(binwidth = 1, geom = "text", aes(label = ..count..), vjust = -0.5) +
    theme(
        axis.title.y = element_blank(),
        axis.title.x = element_blank()
    )

ggsave(glue("report/plots/Treatment Takeup Baseline - Endline.png"), width = 10, height = 5)


##########################################
# Takeup Regression
#########################################

takeup_regression <- function(dat, covariates, outcome) {
    fmla <- reformulate(covariates, outcome)
    res <- lm(fmla, data = dat)
    res
}

outcomes <- list(
    `Used the App` = "has_learning_event",
    `Used More Than 1 Day` = "more_than_one_day_learned",
    `Used More Than 3 Days` = "more_than_three_days_learned"
)


dat <- pooled %>%
    filter(wave == 1) %>%
    filter(treatment == "treated") %>%
    mutate(more_than_one_day_learned = days_learned > 1) %>%
    mutate(more_than_three_days_learned = days_learned > 3) %>%
    replace_na(list(has_learning_event = FALSE, more_than_one_day_learned = FALSE, more_than_three_days_learned = FALSE))

covariates <- c(control_cols, construct_cols)
covariates <- covariates[!(covariates %in% c("was_breastfed", "health_knw"))]

res <- list()

for (outcome in names(outcomes)) {
    var <- outcomes[[outcome]]
    res[[outcome]] <- takeup_regression(dat, covariates, var)
}

covariate_labels <- sapply(covariates, function(n) pretty_vars[[n]])

dep_vars <- names(outcomes)

write_regressions(res, "report/regressions/", "App Usage", dep.var.labels = dep_vars, covariate.labels = covariate_labels, single.row = TRUE, align = TRUE, table.placement = "H")
