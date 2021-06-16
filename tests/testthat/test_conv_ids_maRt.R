library(bioEAT)

test_that("IDs converted true", {
  genes <- c("IFNA1", "IFNA13", "SLC2A3", "ACOT1", "CD45R0", "CD45RA", "CDY2A", "IGHM", "IGKC")
  df <- suppressWarnings(conv_ids_mart(genes, "mmulatta_gene_ensembl", "external_gene_name", c(
    "entrezgene_id",
    "description", "ensembl_gene_id", "external_gene_name"
  )))


  expect_true("IFNA13" %in% df[, 4])
  expect_true("709559" %in% df[, 1])
  expect_true("ENSMMUG00000051298" %in% df[, 3])
  expect_true("IGKC" %in% df[, 4])


  expect_false("ACOT1" %in% df[, 4])
  expect_false("CD45RA" %in% df[, 4])

  expect_equal(nrow(df), 4)

  expect_error(conv_ids_mart(genes, "mmulatta_gene_ensembl", "external_gene_name"))

  expect_error(conv_ids_mart(NA, "mmulatta_gene_ensembl", "external_gene_name", c(
    "entrezgene_id",
    "description", "ensembl_gene_id", "external_gene_name"
  )))
})