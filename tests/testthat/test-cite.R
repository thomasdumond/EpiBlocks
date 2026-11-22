# Tests for the Cite tab (R/cite.R)

test_that("the citation has the version, the address and the accessed date", {
  d <- as.Date("2026-11-02")
  expect_equal(cite_date(d), "2 November 2026")

  txt <- cite_text(d)
  expect_match(txt, "^Dumond T \\(2026\\)\\. EpiBlocks")
  expect_match(txt, "(version 1.0)", fixed = TRUE)
  expect_match(txt, APP_URL, fixed = TRUE)
  expect_match(txt, "(accessed 2 November 2026)", fixed = TRUE)
})

test_that("the BibTeX entry is complete and its braces balance", {
  bib <- cite_bibtex(as.Date("2026-11-02"))
  expect_match(bib, "^@misc\\{epiblocks,")
  for (field in c("author", "title", "year", "version", "howpublished", "note")) {
    expect_match(bib, paste0("\n  ", field, " +="), info = field)
  }
  chars <- strsplit(bib, "")[[1]]
  expect_equal(sum(chars == "{"), sum(chars == "}"))
})
