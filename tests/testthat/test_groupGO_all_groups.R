library(bioEAT)
library("org.Mmu.eg.db")

test_that("groupGO works correctly", {

  test_ens <- c("ENSMMUG00000004852", "ENSMMUG00000055242", "ENSMMUG00000022301", "ENSMMUG00000011153")

  test_eid <- c(
    "716010", "711532", "718802", "574285", "699243", "711832",
    "100187576", "712428", "705671", "699524", "709510", "694372",
    "106996627", "114677644", "2846624", "718073", "710820", "696298",
    "695017", "710577", "698477", "721806", "718964", "698683", "699517",
    "710959", "713687", "702155", "713485", "712934", "711712", "705289",
    "100424758", "710813", "713011", "703429", "100426363", "712657", "705847",
    "574314", "709595", "715454", "707176", "711945", "574097", "704041", "708252",
    "721173", "716211", "694283", "694538", "712365", "714931", "100427850", "574363",
    "698528", "707539", "716296", "714351", "703220", "700795", "707989", "719220", "719253",
    "704332", "718460", "100187587", "2846629", "709860", "709283", "704914", "708080", "693615",
    "721048", "710965", "708406", "720807", "710901", "717726", "717766", "713798", "715354",
    "712101", "693293", "697065", "701691", "708140", "720748", "106996346", "114678757",
    "700603", "716173", "716302", "2846633", "100425941", "721477", "722274", "714697", "704394",
    "718136", "704745", "714851", "718236", "695940", "710795", "722198", "696331", "703534",
    "721010", "712454", "711512", "710590"
  )

  test_out <- groupGO_all_groups(test_eid, "org.Mmu.eg.db", "GO_out", mf = 1, xls = FALSE, pdf = FALSE)

  expect_equal(length(test_out), 3)
  expect_equal(nrow(test_out[[1]]), 31)
  expect_equal(nrow(test_out[[2]]), 979)
  expect_equal(nrow(test_out[[3]]), 1)

  expect_error(groupGO_all_groups(test_eid, org.Mmu.eg.db, "GO_out", mf = 1, xls = FALSE, pdf = FALSE))
  expect_error(groupGO_all_groups(test_eid))
  expect_error(groupGO_all_groups(test_ens, "org.Mmu.eg.db", "GO_out", mf = 1, xls = FALSE, pdf = FALSE))
  expect_error(groupGO_all_groups(NULL, "org.Mmu.eg.db", "GO_out", mf = 1, xls = FALSE, pdf = FALSE))
})