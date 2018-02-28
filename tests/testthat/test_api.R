testthat::test_that("get_info returns valid data", {
  info <- get_info("Q15796")
  expect_equal(is.null(info),F)
})

test_that("get_proteoforms returns valid data",{
  browser()
   proteoforms <- get_proteoforms("Q15796")

  expect_equal(is.null(proteoforms),F)
})

test_that("get_ptmppi returns valid data",{
  ptm_dep_ppi = get_ptm_dependent_ppi("Q15796")
  expect_equal(is.null(ptm_dep_ppi),F)
})


test_that("test get_ppi_for_proteoforms returns valid data",{
  ptm_dep_ppi = get_ppi_for_proteoforms("Q15796")
  expect_equal(is.null(ptm_dep_ppi),F)
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
  expect_equal(is.null(data),F)
})


test_that("get_ptm_enzymes_from_file returns valid data", {
  data = get_ptm_enzymes_from_file("egfr_sites_formatted.txt")
  expect_equal(is.null(data),F)
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
  expect_equal(is.null(data),F)
})

test_that("test get_ptm_ppi_from_file returns valid data", {
  data = get_ptm_ppi_from_file("egfr_sites_formatted.txt")
  expect_equal(is.null(data),F)
})



