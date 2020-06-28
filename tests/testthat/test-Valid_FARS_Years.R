library(testthat)
library(dplyr)

setwd("C:\\Users\\naur9002\\Google Drive\\Trainings\\coursera r\\#3 Building R Packages\\Week 4\\Assignment\\courseraAccFARS\\data-raw")
fars_read(make_filename(2015))

expect_identical(class(fars_read(make_filename(2015))),class(tbl_df(read.csv("accident_2015.csv.bz2"))))
