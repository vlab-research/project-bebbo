source("code/data.R")


normalize <- function(dat, outcome) {
    base <- dat %>% filter(wave == 0)
    m <- mean(base[[outcome]], na.rm=TRUE)
    sd <-  sd(base[[outcome]], na.rm=TRUE)
    
    dat %>% mutate(
                "{outcome}" := (.data[[outcome]] - m) / sd
    )
}


dat <- pooled

# Measure takeup for endline only 
takeup <- dat %>% 
    filter(wave == 1) %>%
    mutate(
        takeup = case_when(
            treatment == "control" ~ "control",
            treatment == "treated" & has_learning_event == FALSE ~ "treated-no-takeup",
            treatment == "treated" & has_learning_event == TRUE ~ "treated-takeup")
    ) %>%
    select(userid, takeup)


endline_users <- dat %>% filter(wave == 1) %>% distinct(userid) %>% pull(userid)

dat <- dat %>% filter(userid %in% endline_users) %>% inner_join(takeup, by = "userid")

dat <- dat %>% filter(wave != 2)

out <- tibble()

for (outcome in construct_cols) {
    dat <- dat %>% normalize(outcome)

    tmp <- dat %>% 
        group_by(takeup, wave) %>% 
        summarise(
            outcome = outcome, 
            m = mean(.data[[outcome]], na.rm=TRUE), 
            var = sd(.data[[outcome]], na.rm=TRUE)**2 / n()
        ) %>% 
        mutate(sd = sqrt(var)) %>% 
        mutate(upper = m + sd*1.96, lower = m - sd*1.96) %>%
        ungroup()
    
    out <- rbind(out, tmp)
}

splits <- split(construct_cols, ceiling(seq_along(construct_cols) / 2))

for (outcomes_ in splits) {

    d <- out %>% filter(outcome %in% outcomes_)

    ggplot(d, aes(x = wave, y = m, color = takeup)) + 
        geom_point() + 
        geom_errorbar(aes(ymin=lower, ymax=upper)) + 
        facet_grid(rows = . ~ outcome + takeup, scale="free_y") + 
        theme(axis.title.x = element_blank(), axis.text.x= element_blank())

    ggsave(glue("report/plots/pre_post/{outcomes_}.png"))
}
