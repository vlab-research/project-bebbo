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
library(RItools)
source("code/functions.R")
source("code/data.R")

folder <- "report/balance"
outcomes <- construct_cols
fmla <- reformulate(outcomes, "treatment")
dat <- pooled

endlined <- pooled %>%
    filter(wave == 1) %>%
    pull(userid)

for (co in c("Serbia", "Bulgaria")) {
    dd <- dat %>%
        filter(country == co)

    bt <- balanceTest(fmla, dd %>% filter(wave == 0))
    res <- data.frame(bt$results) %>%
        select("Control...", "Treatment...", "std.diff...", "z...") %>%
        rename(
            control_mean = "Control...",
            treatment_mean = "Treatment...",
            standardized_diff = "std.diff...",
            z_score = "z..."
        )

    overall_p <- round(bt$overall$p.value, 3)

    write_table(
        res,
        folder,
        glue("Baseline Balance {co}"),
        notes = glue("Overall P-Value: {overall_p}")
    )
}
#############################################################
# Paired t-tests by treatment/control
#############################################################

## demo_cols <- c("parent_age", "number_children")
## all_cols <- c(construct_cols, demo_cols)

## # initialize a matrix to store results
## t.test.serbia <- matrix(nrow = length(all_cols), ncol = 9)
## # baseline treated
## treated.serbia <- serbia %>% filter(endline == 0 && treatment == "treated")
## # baseline control
## control.serbia <- serbia %>% filter(endline == 0 && treatment == "control")

## i <- 1
## for (col in all_cols) {
##     test <- t.test(x = treated.serbia[[col]], y = control.serbia[[col]], mu = 0, paired = FALSE, var.equal = TRUE)
##     t.test.serbia[i, 1] <- col
##     t.test.serbia[i, 2:9] <- round(cbind(test$estimate[1], test$estimate[2], test$statistic, test$p.value, test$stderr, test$conf.int[1], test$conf.int[2], test$parameter), 3)
##     i <- i + 1
##     rm(test)
## }

## t.test.serbia <- as.data.table(t.test.serbia)
## colnames(t.test.serbia) <- c("variable", "treatment mean", "control mean", "test statistic", "p-val", "std.error", "conf.int lower", "conf.int upper", "df")
## t.test.serbia <- t.test.serbia[order(`p-val`)]
## write_table(t.test.serbia, "treatment_baseline_imbalance_serbia")

## # initialize a matrix to store results
## t.test.bulgaria <- matrix(nrow = length(all_cols), ncol = 9)
## # baseline treated
## treated.bulgaria <- bulgaria %>% filter(endline == 0 && treatment == "treated")
## # baseline control
## control.bulgaria <- bulgaria %>% filter(endline == 0 && treatment == "control")

## i <- 1
## for (col in all_cols) {
##     print(col)
##     test <- t.test(x = treated.bulgaria[[col]], y = control.bulgaria[[col]], mu = 0, paired = FALSE, var.equal = TRUE)
##     t.test.bulgaria[i, 1] <- col
##     t.test.bulgaria[i, 2:9] <- round(cbind(test$estimate[1], test$estimate[2], test$statistic, test$p.value, test$stderr, test$conf.int[1], test$conf.int[2], test$parameter), 3)
##     i <- i + 1
##     rm(test)
## }

## t.test.bulgaria <- as.data.table(t.test.bulgaria)
## colnames(t.test.bulgaria) <- c("variable", "treatment mean", "control mean", "test statistic", "p-val", "std.error", "conf.int lower", "conf.int upper", "df")
## t.test.bulgaria <- t.test.bulgaria[order(`p-val`)]
## write_table(t.test.bulgaria, "treatment_baseline_imbalance_bulgaria")


## #############################################################
## # Paired t-tests by country
## #############################################################

## # initialize a matrix to store results
## t.test.matrix <- matrix(nrow = length(all_cols), ncol = 9)
## # baseline serbia
## tc.serbia <- serbia %>% filter(endline == 0)
## # baseline bulgaria
## tc.bulgaria <- bulgaria %>% filter(endline == 0)

## i <- 1
## for (col in all_cols) {
##     print(col)
##     test <- t.test(x = tc.serbia[[col]], y = tc.bulgaria[[col]], mu = 0, paired = FALSE, var.equal = TRUE)
##     t.test.matrix[i, 1] <- col
##     t.test.matrix[i, 2:9] <- round(cbind(test$estimate[1], test$estimate[2], test$statistic, test$p.value, test$stderr, test$conf.int[1], test$conf.int[2], test$parameter), 3)
##     i <- i + 1
##     rm(test)
## }

## t.test.matrix <- as.data.table(t.test.matrix)
## colnames(t.test.matrix) <- c("variable", "serbia mean", "bulgaria mean", "test statistic", "p-val", "std.error", "conf.int lower", "conf.int upper", "df")
## t.test.matrix <- t.test.matrix[order(`p-val`)]
## write_table(t.test.matrix, "country_baseline_constructs_imbalance")

## # #############################################################
## # # Paired t-tests by baseline/endline
## # #############################################################
## #
## # #initialize a matrix to store results
## # t.test.endline<-matrix(nrow=length(all_cols),ncol=8)
## #
## # #baseline/endline serbia
## # tc.serbia.baseline<-serbia%>%filter(endline==0)
## # tc.serbia.endline<-serbia%>%filter(endline==1)
## #
## # #baseline/endline serbia
## # tc.bulgaria.baseline<-bulgaria%>%filter(endline==0)
## # tc.bulgaria.endline<-bulgaria%>%filter(endline==1)
## #
## # i=1
## # for(col in all_cols){
## #   print(col)
## #   test<-t.test(x=tc.serbia.baseline[[col]],y=tc.serbia.endline[[col]],mu=0,paired=FALSE,var.equal=TRUE)
## #   t.test.endline[i,1]  <-col
## #   t.test.endline[i,2:8]<-round(cbind(test$estimate[1],test$estimate[2],test$statistic,test$p.value,test$stderr,test$conf.int[1],test$conf.int[2]),3)
## #   i=i+1
## #   rm(test)
## # }
## #
## # t.test.endline<-as.data.table(t.test.endline)
## # colnames(t.test.endline)<-c('variable','baseline mean','endline mean','test statistic','p-val','std.error','conf.int lower','conf.int upper')
## # t.test.endline<-t.test.endline[order(`p-val`)]
## # write_table(t.test.endline,'endline_baseline_constructs_imbalance')
## #
## #
