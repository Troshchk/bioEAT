library(bioEAT)
library("openxlsx")

test_that("Types are detected correctly", {

  df <- read.xlsx("/Users/kseniatroshchenkova/Downloads/mRNA_normalized_CPM.xlsx", rowNames = TRUE)
  test_df <- head(df, 10)
  colnames(test_df)[1:2] <- c("test1", "test2")

  expect_error(test_input_type(test_df, "1"))
  expect_error(test_input_type(test_df, 200))
  expect_error(test_input_type(test_df, "a"))

  expect_silent(test_input_type(test_df, "test1"))
  expect_silent(test_input_type(test_df, 1))


})