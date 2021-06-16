library(bioEAT)

test_that("IDs converted true", {
  genes <- c("IFNA1", "IFNA13", "SLC2A3", "ACOT1", "CD45R0", "CD45RA", "CDY2A","IGHM", "IGKC")
  df <- suppressWarnings(conv_ids_cp(genes, "SYMBOL", c("ENTREZID", "ENSEMBL"), "org.Mmu.eg.db"))

  expect_true("IFNA13" %in% df[, 1])
  expect_true("709559" %in% df[, 2])
  expect_true("ENSMMUG00000051298" %in% df[, 3])
  

  expect_false("IGKC" %in% df[, 1])
  expect_false("ACOT1" %in% df[, 1])
  expect_false("CD45RA" %in% df[, 1])

  expect_equal(nrow(df),2)

  expect_error(conv_ids_cp(genes, "SYMBOL", db = "org.Mmu.eg.db"))

  expect_error(conv_ids_cp(NA, "SYMBOL", c("ENTREZID", "ENSEMBL"), "org.Mmu.eg.db"))
})