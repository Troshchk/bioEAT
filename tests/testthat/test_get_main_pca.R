library(bioEAT)
library("openxlsx")

test_that("PCA components returned correctly", {

  main3 <- c("ENSMMUG00000062077", "ENSMMUG00000004852","ENSMMUG00000055242")
  main5 <- c("ENSMMUG00000062077", "ENSMMUG00000004852", "ENSMMUG00000055242", "ENSMMUG00000011153", "ENSMMUG00000060797")

  df <- read.xlsx("../test_df.xlsx", rowNames = TRUE)

  expect_equal(length(get_main_pca(df, 3)), 3)
  expect_setequal(get_main_pca(df, 3), main3)

  expect_error(get_main_pca(df, ))
  expect_error(get_main_pca(NA, 3))
  expect_error(get_main_pca(df, -3))
  expect_error(get_main_pca(df, a))
  expect_error(get_main_pca(df, "a"))

  expect_equal(length(get_main_pca(df, 5)), 5)
  expect_setequal(get_main_pca(df, 5), main5)
})