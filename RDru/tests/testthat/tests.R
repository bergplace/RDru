# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(RDru)

test_that("Handling wrong addresses", {

  string <- "www.google.com"

  expect_error(dru_start(string))
  expect_error(dru_get_blocks(string, 0, 0))
  expect_error(dru_get_blocks_reduced(string, 0, 0))
  expect_error(dru_get_edges(string, 0, 0))
  expect_error(dru_get_degree(string, 1000, 1010, "all"))
  expect_error(dru_get_degree_by_block(string, 1, 10,"t1KstPVzcNEK4ZeauQ6cogoqxQBMDSiRnGr", "all")) #nolint
  expect_error(dru_get_degree_max(string, 10000, 10010, "all"))
  expect_error(dru_get_betweenness(string, 0, 1000, "true"))
  expect_error(dru_get_betweenness_max(string, 0, 1000, "true"))
  expect_error(dru_get_closeness(string, 0, 100, "true"))
  expect_error(dru_get_closeness_max(string, 0, 100, "true"))
  expect_error(dru_get_transitivity(string, 0, 1000))
  expect_error(dru_get_transitivity_global(string, 0, 1000))
  expect_error(dru_get_diameter(string, 0, 1000, "true"))
  expect_error(dru_get_density(string, 0, 1000, "true", "true"))
  expect_error(dru_are_connected(string, 0, 100, "t1StbPM4X3j4FGM57HpGnb9BMbS7C1nFW1r", "t1KstPVzcNEK4ZeauQ6cogoqxQBMDSiRnGr", "true")) #nolint
  expect_error(dru_get_transactions_value(string, 0, 1000, "t1StbPM4X3j4FGM57HpGnb9BMbS7C1nFW1r", "t1KstPVzcNEK4ZeauQ6cogoqxQBMDSiRnGr")) #nolint
  expect_error(dru_get_zcash_tx_types_count(string, 10010, 10020))
})