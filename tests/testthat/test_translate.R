library(bioEAT)
library("openxlsx")

test_that("IDs are translated correctly", {

  df <- read.xlsx("../test_df.xlsx", rowNames = TRUE)
  test_df <- head(df, 10)


  homologene_table <- read.delim("../homologene.data", header = FALSE)

  to_homo <- suppressWarnings(translate(rownames(test_df), 9606,
    from_id = "ENSEMBL", homologeneFile = homologene_table,
    db_cluster_profiler = "org.Mmu.eg.db", mart = "mmulatta_gene_ensembl", from_mart = "ensembl_gene_id"
  ))


  to_homo_from_ent <- suppressWarnings(translate(to_homo$ENTREZID_input, 9606, homologeneFile = homologene_table))

  expect_error(translate(rownames(test_df), 9544,
    from_id = "ENSEMBL", homologeneFile = random_file,
    db_cluster_profiler = "org.Mmu.eg.db", mart = "mmulatta_gene_ensembl",
    from_mart = "ensembl_gene_id"
  ))

  expect_error(translate(rownames(test_df), 9544,
    from_id = "ENSEMBL", homologeneFile = homologene_table,
    mart = "mmulatta_gene_ensembl",
    from_mart = "ensembl_gene_id"
  ))

  expect_error(translate(rownames(test_df), 9544,
    from_id = "ENSEMBL", homologeneFile = homologene_table,
    db_cluster_profiler = "org.Mmu.eg.db",
    from_mart = "ensembl_gene_id"
  ))

  expect_error(translate(rownames(test_df), 9544,
    from_id = "ENSEMBL", homologeneFile = homologene_table,
    db_cluster_profiler = "org.Mmu.eg.db", mart = "mmulatta_gene_ensembl"
  ))

  expect_equal(nrow(to_homo), 14)
  expect_equal(to_homo[1,1], "100187576")
  expect_equal(to_homo[4,2], "ENSMMUG00000011153")


  expect_equal(nrow(to_homo_from_ent), 14)
  expect_equal(to_homo_from_ent[1,1], "100187576")
  expect_equal(to_homo_from_ent[4,2], "60")


})