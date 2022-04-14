library(mars)
load("testpredict.RData")
test_that("predict.mars() returns the correct predictions on the same data used to fit the model", {
  expect_equal(predict.mars(testmars), testpredict,ignore_attr=TRUE)} )
test_that("predict.mars() returns the correct predictions on new data", {
  expect_equal(predict.mars(testmars,newdata=marstestdata), testpredict,ignore_attr=TRUE)} )
