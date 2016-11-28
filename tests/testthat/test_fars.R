test_that("load the fars data", {
  data_file<-system.file("extdata","accident_2013.csv.bz2", package = "farsdata")
  setwd(dirname(data_file))
  file="accident_2013.csv.bz2"
  dat<-fars_read(file)
  expect_equal(dat$PERMVIT[1], 8)
  expect_equal(dat$CF1[11], 20)
})


test_that("load the fars data", {
  data_file<-system.file("extdata","accident_2013.csv.bz2", package = "farsdata")
  setwd(dirname(data_file))
  dat2=fars_summarize_years(c(2013,2014))
  expect_equal(dat2[[2]][1], 2230)
  expect_equal(dat2[[3]][5], 2596)
})
