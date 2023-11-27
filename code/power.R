library(ggplot2)
library(pwr)
source("code/data.R")

power_per_size <- function(n, d, sig) {
    takeup <- 0.28
    res <- pwr.t.test(n = n, d = d*takeup, sig.level = sig)
    res$power
}

get_n <- function(dat) {
    dat %>% 
        filter(wave == 1) %>% 
        group_by(treatment) %>% 
        count() %>% 
        pull(n) %>%
        min()
}


datasets <- list(
    `Serbia` = serbia,
    `Bulgaria` = bulgaria_with_impacted,
    `Pooled` = pooled
)

df <- tibble()

for (dataset in names(datasets)) {
    dat <- datasets[[dataset]]
    n <- get_n(dat)

    for (z in c(0.10/8, 0.10)) {
        x <- seq(0, .8, 0.025)
        y <- sapply(x, function (d) power_per_size(n, d, z))
        d <- tibble(x, y)
        d <- mutate(d, z=z, Dataset = dataset)
        df <- rbind(df, d)
    }        
}


df <- df %>% 
    mutate(z = factor(z)) %>% 
    rename(`Power` = y, `Effect Size` = x, `Significance` = z)

ggplot(df, aes(x=`Effect Size`, y=Power, color=Dataset)) +
    geom_line() + 
    facet_grid(cols = vars(Significance))




##############################################
# What effect size is possible given baseline? 
##############################################

dat <- pooled

difs <- make_outcome_difs(dat, construct_cols)

sd_lookup <-  difs %>% 
    filter(wave == 1) %>% 
    select(construct_cols) %>% 
    summarise(across(all_of(construct_cols), ~ sd(.x, na.rm=T))) %>% 
    pivot_longer(cols=construct_cols) %>% 
    rename(sd = value)

max_lookup <-  dat %>% 
    filter(wave == 0) %>% 
    select(construct_cols) %>% 
    summarise(across(all_of(construct_cols), ~ max(.x, na.rm=T))) %>% 
    pivot_longer(cols=construct_cols) %>% 
    rename(max = value)

d <- dat %>% 
    select(userid, wave, treatment, construct_cols) %>% 
    pivot_longer(cols = construct_cols) %>% 
    filter(wave == 0) %>%
    inner_join(sd_lookup, by="name") %>%
    mutate(name = factor(name, levels = construct_cols))

ggplot(d, aes(x=value, y = after_stat(density), color=treatment)) + 
    geom_histogram(position="identity", alpha = 0.2) + 
    facet_wrap(vars(name), nrow=4, ncol=2)






# these are standard deviations of Y for regression. 
# 0.5 * sd is the medium effect size.
 
d <- difs %>% 
    select(userid, wave, treatment, construct_cols) %>% 
    pivot_longer(cols = all_of(construct_cols)) %>% 
    filter(wave == 1) %>%
    inner_join(sd_lookup, by="name") %>%
    mutate(name = factor(name, levels = construct_cols)) %>%
    mutate(normalized_value = value / sd) 

means <- d %>% group_by(name, treatment) %>% summarise(mean = mean(value, na.rm=T))

d <- d %>% inner_join(means, by=c("name", "treatment"))

ggplot(d, aes(x=normalized_value, y = after_stat(density), color=treatment)) + 
    geom_histogram(position="identity", alpha = 0.2) + 
    geom_vline(aes(xintercept = mean, color = treatment)) + 
    ## geom_vline(aes(xintercept = 0.3*0.5)) + 
    ## geom_vline(aes(xintercept = -0.3*0.5)) + 
    facet_wrap(vars(name), nrow=4, ncol=2)
