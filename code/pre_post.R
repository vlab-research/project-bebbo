source("code/data.R")


normalize <- function(dat, outcome) {
    base <- dat %>% filter(endline == 0)
    m <- mean(base[[outcome]], na.rm=TRUE)
    sd <-  sd(base[[outcome]], na.rm=TRUE)
    
    dat %>% mutate(
                "{outcome}" := (.data[[outcome]] - m) / sd
    )
}


dat <- pooled

dat <- dat |> mutate(takeup = case_when(
                         treatment == "control" ~ "control",
                         treatment == "treated" & has_learning_event ==FALSE ~ "treated-no-takeup",
                         treatment == "treated" & has_learning_event ==TRUE ~ "treated-takeup",

))

endline_users <- dat %>% filter(endline == 1) %>% distinct(userid) %>% pull(userid)

dat <- dat %>% filter(userid %in% endline_users)

dat <- dat %>% filter(endline != 2)


out <- tibble()

for (outcome in construct_cols) {
    dat <- dat %>% normalize(outcome)

    tmp <- dat %>% 
        group_by(takeup, endline) %>% 
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

outcomes_ <- construct_cols[6:7]

out <- out %>% filter(outcome %in% outcomes_)


## ggplot(out, aes(x = endline, y = m, color = takeup)) + 
##     geom_point() + 
##     geom_line() + 
##     geom_errorbar(aes(ymin=lower, ymax=upper))


ggplot(out, aes(x = endline, y = m, color = takeup)) + 
    geom_point() + 
    geom_errorbar(aes(ymin=lower, ymax=upper)) + 
    facet_grid(rows = . ~ outcome + takeup, scale="free_y") + 
    theme(axis.title.x = element_blank(), axis.text.x= element_blank())
