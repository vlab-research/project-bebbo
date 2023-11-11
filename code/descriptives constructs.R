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
source("code/functions.R")
source("code/data.R")


#############################################################
# Read Data
#############################################################
# In this section, we read and perform some pre-processing for the datasets we will require for this analysis


#############################################################
# Descriptives
#############################################################
# In this section, we perform the following analyses for
# baseline vs endline and across both countries, serbia and bulgaria
# 1. Baseline Descriptives - Serbia
# 2. Baseline Descriptives - Bulgaria
# 3. Endline Descriptives - Serbia
# 4. Endline Descriptives - Bulgaria
# 5. Correlations - Serbia
# 6. Correlations - Bulgaria


table_folder <- "report/descriptives/tables"
plot_folder <- "report/descriptives/plots"

write_descriptive_tables <- function(dat, country, endline_flag) {
    suffix <- ifelse(endline_flag == 0, "Baseline", "Endline")

    constructs <- key[, c("Domain", "Subdomain", "construct_variable", "variable", "Grouping of constructs")] %>%
        mutate(
            Subdomain = case_when(is.na(Subdomain) ~ `Grouping of constructs`, TRUE ~ Subdomain),
            `Grouping of constructs` = NULL
        ) %>%
        drop_na(construct_variable) %>%
        arrange(Domain, Subdomain, construct_variable, variable)

    # 1-4a. Likert Variables - Most granular summary of likert variables
    descrip_variable_likert <- dat %>%
        filter(endline == endline_flag) %>%
        select(all_of(likert_cols)) %>%
        descriptives(type = "likert") %>%
        merge(constructs[, c("variable", "Subdomain")], by.x = "name", by.y = "variable", all.x = TRUE) %>%
        relocate(Subdomain, .before = "name") %>%
        filter(!is.na(Subdomain)) %>%
        arrange(Subdomain)

    # 1-4b. Binary Variables - Most granular summary of binary variables
    descrip_variable_binary <- dat %>%
        filter(endline == endline_flag) %>%
        select(all_of(binary_cols)) %>%
        descriptives(type = "binary") %>%
        merge(constructs[, c("variable", "Subdomain")], by.x = "name", by.y = "variable", all.x = TRUE) %>%
        relocate(Subdomain, .before = "name") %>%
        arrange(Subdomain)

    # 1-4c. Construct Variables
    descrip_construct <- dat %>%
        filter(endline == endline_flag) %>%
        select(all_of(construct_cols)) %>%
        descriptives(type = "summary") %>%
        merge(constructs[, c("construct_variable", "Subdomain")], by.x = "name", by.y = "construct_variable", all.x = TRUE) %>%
        distinct() %>%
        relocate(Subdomain, .before = "name") %>%
        arrange(Subdomain)


    pal <- colorRampPalette(c("red", "navy"))


    dat %>%
        filter(endline == endline_flag) %>%
        select(all_of(construct_cols)) %>%
        lowerCor() %>%
        corrplot(method = "number", number.cex = 1, type = "lower", col = pal(2), tl.cex = 0.8)


    dev.print(
        device = jpeg,
        filename = paste0(plot_folder, "/", "correlations_constructs_", country, "_", suffix, ".jpg"),
        width = 800,
        height = 800
    )



    write_table(constructs, table_folder, glue("Constructs {suffix}"), align = TRUE)
    write_table(descrip_variable_likert, table_folder, glue("Likert Variable Descriptives {country} {suffix}"), align = TRUE)
    write_table(descrip_variable_binary, table_folder, glue("Binary Variable Descriptives {country} {suffix}"), align = TRUE)
    write_table(descrip_construct, table_folder, glue("Outcome Construct Descriptives {country} {suffix}"), align = TRUE)

    # return tables??
}


# 1. Baseline Descriptives - Serbia
write_descriptive_tables(serbia, "Serbia", 0)

# 2. Baseline Descriptives - Bulgaria
write_descriptive_tables(bulgaria, "Bulgaria", 0)

# 3. Endline Descriptives - Serbia
write_descriptive_tables(serbia, "Serbia", 1)

# 4. Endline Descriptives - Bulgaria
write_descriptive_tables(bulgaria, "Bulgaria", 1)


domains_constructs_variables %>% write_table(table_folder, "Construct Variable Mapping")
