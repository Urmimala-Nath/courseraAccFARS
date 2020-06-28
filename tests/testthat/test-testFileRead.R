
#LOADING TESTING LIBRARY
library(testthat)

#TO CHECK IF ERROR GENERATED
expect_error(fars_read(make_filename(2020)),regexp = "file 'accident_2020.csv.bz2' does not exist")

#TO CHECK IF VALUE MATCHES
expect_identical(make_filename(2013),'accident_2013.csv.bz2')
