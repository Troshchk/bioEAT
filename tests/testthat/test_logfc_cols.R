library(bioEAT)
library("openxlsx")

test_that("LOGfc is calculate corectly", {

  df <- read.xlsx("../test_df.xlsx", rowNames = TRUE)
  test_df <- head(df, 10)
  colnames(test_df)[1:2] <- c("test1", "test2")

  df_out <- logfc_cols(test_df, 1, 2)
  expect_equal(df_out[1,3], -0.008367353, tolerance=1e-7)
  expect_equal(df_out[10,3], -0.828825539, tolerance=1e-9)

  df_out <- logfc_cols(test_df, 2, 1)
  expect_equal(df_out[1,3], 0.008367353, tolerance=1e-7)
  expect_equal(df_out[10,3], 0.828825539, tolerance=1e-9)


  df_out <- logfc_cols(test_df, 2, 6)
  expect_equal(df_out[10,3], 0.7619902, tolerance=1e-7)
  

  expect_error(logfc_cols(test_df, "1", "2"))
  expect_error(logfc_cols(test_df, 1, "2"))
  expect_error(logfc_cols(test_df, 1, 200))
  expect_error(logfc_cols(test_df, "1", 2))
  expect_error(logfc_cols(test_df, "1", ))
  expect_error(logfc_cols(test_df, , "2"))

})