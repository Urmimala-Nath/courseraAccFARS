library(testthat)
library(dplyr)

setwd("C:\\Users\\naur9002\\Google Drive\\Trainings\\coursera r\\#3 Building R Packages\\Week 4\\Assignment\\courseraAccFARS\\data-raw")

val<-fars_summarize_years(c(2012,2013,2015,2020))

expect_equal(colnames(val),c("MONTH","2013","2015"))
