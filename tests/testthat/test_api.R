test_that("get_info returns valid data", {
  data = get_info("Q15796")
  expect_equal(is.null(data),F)
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




