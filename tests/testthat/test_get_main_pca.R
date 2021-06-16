library(bioEAT)
library("openxlsx")

test_that("PCA components returned correctly", {

  main3 <- c("ENSMMUG00000062077", "ENSMMUG00000051300","ENSMMUG00000055690")
  main5 <- c("ENSMMUG00000062077", "ENSMMUG00000051300", "ENSMMUG00000055690", "ENSMMUG00000014899", "ENSMMUG00000004852")

  df <- read.xlsx("/Users/kseniatroshchenkova/Downloads/mRNA_normalized_CPM.xlsx", rowNames = TRUE)

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