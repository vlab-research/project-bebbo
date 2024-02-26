library(readr)
library(lubridate)


learning_events <- c(
    "advise_details_opened",
    "game_details_opened",
    "child_milestone_tracked",
    "child_measurement_entered",
    "child_vaccine_entered",
    "child_health_checkup_entered"
)


parse_bq_date <- function(i) {
    as.POSIXct(i / 1000 / 1000)
}

rollup_events <- function(events, variables) {
    events %>%
        filter(event_name %in% learning_events) %>%
        group_by(across(all_of(c("userid", variables)))) %>%
        summarize(
            learning_events = n(),
            days_learned = n_distinct(event_day),
        ) %>%
        mutate(
            has_learning_event = if_else(is.na(learning_events), FALSE, learning_events > 0),
        )
}


raw_app <- read_csv("data/raw/bq-results-20231123.csv")

app_events <- raw_app %>%
    mutate(event_timestamp = parse_bq_date(event_timestamp)) %>%
    mutate(
        event_day = date(event_timestamp),
        userid = facebook_id
    )


d <- rollup_events(app_events, c())

d %>% ggplot(aes(x = days_learned)) +
    geom_histogram()
