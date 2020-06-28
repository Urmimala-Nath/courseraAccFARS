library(courseraAccFARS)
library(testthat)
make_filename(2017)
expect_identical(make_filename(2014),"accident_2014.csv.bz2")
