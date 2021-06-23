library(bioEAT)
library("openxlsx")

test_that("Shannon entropy is calculated correctly", {
  df <- read.xlsx("../test_df.xlsx", rowNames = TRUE)

  sh_plots <- sh_entropy(df, pdf = FALSE)

  expect_equal(nrow(sh_plots$Entropy_values), 30)
  expect_equal(length(sh_plots), 3)

  expect_equal(sh_plots$Entropy_values[1, 2], 3.20305403037269, tolerance = 1e-7)
  expect_equal(sh_plots$Entropy_values[27, 2], 3.21057953832458, tolerance = 1e-7)


  expect_error(sh_entropy(df_not_existing, pdf = FALSE))
  expect_error(sh_entropy(df, relevel = TRUE, levels = c(1, 2, 3), pdf = FALSE))
  expect_error(sh_entropy(df, relevel = TRUE, levels = c(seq(5,ncol(df),1),1,1,1,1), pdf = FALSE))

  df[1, 2] <- "test"
  expect_error(sh_entropy(df, pdf = FALSE))

})