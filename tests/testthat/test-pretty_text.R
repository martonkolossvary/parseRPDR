###############################################################################
## Title: Test pretty_text() function
## Project: parseRPDR
## Description: Test pretty_text() function
## Copyright: Márton Kolossváry, MD, PhD
## Date: 2023-02-24
###############################################################################

testthat::skip_if_offline()
# Load and check equality ====================
suppressPackageStartupMessages(library(parseRPDR))

test_that("pretty_text() - remove after", {
  expect_equal("abc", pretty_text("abc-zyx", remove_after = TRUE))
})

test_that("pretty_text() - remove punctuation", {
  expect_equal("abc", pretty_text("abc.!", remove_punc = TRUE))
})

test_that("pretty_text() - remove punctuation", {
  expect_equal("abc", pretty_text("abc   ", remove_white = TRUE))
})

