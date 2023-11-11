library(ivreg)
source("code/functions.R")
source("code/data.R")


format_for_reg <- function(dat, outcome) {
    baselines <- dat %>%
        filter(endline == 0) %>%
        select(userid, construct_cols)

    baseline <- baselines %>%
        select(userid, {{ outcome }}) %>%
        rename(baseline_value = {{ outcome }})

    dat %>%
        filter(endline == 1) %>%
        inner_join(baseline, by = "userid")
}


pooled <- dat <- rbind(serbia_with_impacted, bulgaria_with_impacted)

datasets <- list(
    `Serbia` = serbia
)

outcomes <- construct_cols
controls <- control_cols[!(control_cols %in% c("age_flag"))]

run_regression <- function(dat, outcome, ...) {
    fmla <- reformulate(c("treatment", "baseline_value", controls, ...), outcome)
    dat <- format_for_reg(dat, outcome)
    model <- lm(fmla, dat, na.action = na.omit)
    model
}


run_tot_regression <- function(dat, outcome) {
    control_formula <- paste0(controls, collapse = " + ")
    endog <- "has_learning_event"

    ff <- glue("{outcome} ~ {endog} + baseline_value + {control_formula} | treatment + baseline_value + {control_formula}")
    fmla <- formula(ff)    
    model <- ivreg(fmla, data = format_for_reg(dat, outcome))
    model
}


adjust_p_value <- function(p_values, p_value) {
    adjusted <- p.adjust(flatten(p_values), method = "BH")
    adjusted <- sapply(adjusted, function(x) format(x, digits=3))
    flattened <- as.numeric(flatten(p_values))
    l <- list()
    for (i in seq_along(flattened)) {
        if (flattened[[i]] == p_value) {
            return(adjusted[[i]])
        }
    }
}

get_p_values <- function(model) {
    summary(model)$coefficients[, 4]
}

get_p_value <- function(model, var) {
    summary(model)$coefficients[, 4][var]
}

get_diagnostic_p <- function(model, diagnostic) {
    p <- summary(model)$diagnostics[diagnostic, 4]
    format(p, digits=3)
}

# TODO: Add "caregiver_well_being" back when it's finalized!

domains <- list(
    `Knowledge and Awareness` = c("health_knw", "dev_knw_recog"),
    `Confidence and Attitudes` = c("confidence", "attitude"),
    `Practices` = c("was_breastfed", "practices_24", "practices_agree", "practices_hostility")
)

models <- list(
    `OLS` = run_regression,
    `2SLS` = run_tot_regression
)

for (model in names(models)) {
    fun <- models[[model]]
    
    for (dataset in names(datasets)) {
        dat <- datasets[[dataset]]
        res <- lapply(domains, function(outcomes) {
            if (dataset == "Serbia") {
                return(lapply(outcomes, function(o) fun(dat, o)))
            }
            if (dataset == "Pooled") {
                return(lapply(outcomes, function(o) fun(dat, o, "country")))
            }
        })

        var_of_interest <- if_else(model == "2SLS", "has_learning_eventTRUE", "treatmenttreated")

        p_values <- lapply(res, function(x) lapply(x, function(y) get_p_value(y, var_of_interest)))

        for (name in names(res)) {
            results <- res[[name]]

            local_p_values <- sapply(results, function(m) adjust_p_value(p_values, get_p_value(m, var_of_interest)))

            if (model == "2SLS") {
                lines <- list(
                    c("Adjusted Treatment p-value", local_p_values),
                    c("Weak instruments p-value", sapply(results, function(r) get_diagnostic_p(r, "Weak instruments"))),
                    c("Wu-Hausman p-value", sapply(results, function(r) get_diagnostic_p(r, "Wu-Hausman")))
                )
            }
            if (model == "OLS") {
                lines <- list(
                    c("Adjusted Treatment p-value", local_p_values)
                )
            }
            
            print(sapply(results, get_weak_instruments_p))
            
            write_regressions(
                results,
                "report/regressions",
                glue("{dataset}: {model} {name}"),
                add.lines = lines,
                ## style = "all",
                ## p = local_p_values,
                ## omit = c(control_cols),
                keep.stat = c("n", "rsq")
            )
        }
    }
}







# 1 Home Open, Home Open + 1 of the 6 key events, 1 + of the 6 key events, 2 Home Opens
# >0 key events

serbia <- serbia %>% left_join(app_usage, by = "userid")
serbia <- serbia %>% mutate(has_learning_event = if_else(is.na(learning_events), FALSE, learning_events > 0))








