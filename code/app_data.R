library(readr)
library(lubridate)

parse_bq_date <- function(i) {
    as.POSIXct(i / 1000 / 1000)
}

raw_app <- read_csv("data/raw/bq-results-20230725.csv")

app_usage <- raw_app %>%
    mutate(event_timestamp = parse_bq_date(event_timestamp)) %>%
    group_by(facebook_id) %>%
    summarize(
        facebook_id = first(facebook_id),
        session_starts = sum(event_name == "session_start"),
        screen_views = sum(event_name == "screen_view"),
        user_engagements = sum(event_name == "user_engagement"),
        home_opens = sum(event_name == "Home_opened"),
        milestones_tracked = sum(event_name == "child_milestone_tracked"),
        time_used = max(event_timestamp) - min(event_timestamp)
    )
