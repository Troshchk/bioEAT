library(bioEAT)
library("org.Mmu.eg.db")

genes <- c("IFNA1", "IFNA13", "SLC2A3", "ACOT1", "CD45R0", "CD45RA", "CDY2A", "IGHM", "IGKC")

test_that("IDs converted true", {
  genes <- c("IFNA1", "IFNA13", "SLC2A3", "ACOT1", "CD45R0", "CD45RA", "CDY2A", "IGHM", "IGKC")
  df <- suppressWarnings(conv_ids_full(genes, "org.Mmu.eg.db", "SYMBOL", "mmulatta_gene_ensembl", "external_gene_name"))

  expect_setequal(df[, 2], genes)

  expect_equal(nrow(df), length(genes))

  expect_error(conv_ids_full(NA, "org.Mmu.eg.db", "SYMBOL", "mmulatta_gene_ensembl", "external_gene_name"))
})


test_that("IDs converted true", {
  
  genes <- c("SLC2A3", "ACOT1", "CD45R0", "CD45RA", "CDY2A", "IGHM", "IGKC")
  df <- suppressWarnings(conv_ids_full(genes, "org.Mmu.eg.db", "SYMBOL", "mmulatta_gene_ensembl", "external_gene_name"))

  expect_setequal(df[, 2], genes)

  expect_equal(nrow(df), length(genes))
})