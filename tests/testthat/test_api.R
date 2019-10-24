library(iptmnetr)
context("iTPMnet API testing")

#set_host_url("http://127.0.0.1:8080")


testthat::test_that("get_info returns valid data", {
  info <- get_info("Q15796")
  expect_equal(is.null(info),F)
})

testthat::test_that("search returns valid data", {
  search_results <- search_iptmnet("Smad2",TermType()$ALL,Role()$EnzymeOrSubstrate)
  row_count <- nrow(search_results)
  cd <- row_count != 0
  expect_equal(cd,TRUE)
})

test_that("get_substrates returns valid data",{
  substrates <- get_substrates("Q15796")
  row_count <- nrow(substrates)
  cd <- row_count != 0
  expect_equal(cd,TRUE)
})

test_that("get_proteoforms returns valid data",{
  proteoforms <- get_proteoforms("Q15796")
  row_count <- nrow(proteoforms)
  cd <- row_count != 0
  expect_equal(cd,TRUE)
})

test_that("get_ptmppi returns valid data",{
  ptm_dep_ppi = get_ptm_dependent_ppi("Q15796")
  row_count <- nrow(ptm_dep_ppi)
  expect_equal(row_count != 0,TRUE)
})

test_that("get_variants returns valid data",{
  variants = get_variants("Q15796")
  row_count <- nrow(variants)
  expect_equal(row_count != 0,TRUE)
})

test_that("test get_ppi_for_proteoforms returns valid data",{
  ppi = get_ppi_for_proteoforms("Q15796")
  row_count <- nrow(ppi)
  expect_equal(row_count != 0,TRUE)
})

test_that("get_ptm_enzymes_from_list returns valid data", {
  substrates = list(
    list(
      substrate_ac="Q15796",
      site_residue="K",
      site_position="19"
    ),
    list(
      substrate_ac="Q15796",
      site_residue="T",
      site_position="8"
    ),
    list(
      substrate_ac="P04637",
      site_residue="K",
      site_position="120"
    )
  )
  data = get_ptm_enzymes_from_list(substrates)
  row_count <- nrow(data)
  expect_equal(row_count != 0,TRUE)
})

test_that("get_ptm_enzymes_from_file returns valid data", {
  skip_on_cran()
  data = get_ptm_enzymes_from_file("egfr_sites_formatted_long.txt")
  row_count <- nrow(data)
  expect_equal(row_count != 0,TRUE)
})

test_that("test get_ptm_ppi_from_list returns valid data", {
  substrates = list(
    list(
      substrate_ac="Q15796",
      site_residue="K",
      site_position="19"
    ),
    list(
      substrate_ac="Q15796",
      site_residue="T",
      site_position="8"
    ),
    list(
      substrate_ac="P04637",
      site_residue="S",
      site_position="378"
    )
  )
  data = get_ptm_ppi_from_list(substrates)
  row_count <- nrow(data)
  expect_equal(row_count != 0,TRUE)
})

test_that("test get_ptm_ppi_from_file returns valid data", {
  skip_on_cran()
  data = get_ptm_ppi_from_file("egfr_sites_formatted.txt")
  row_count <- nrow(data)
  expect_equal(row_count != 0,TRUE)
})



