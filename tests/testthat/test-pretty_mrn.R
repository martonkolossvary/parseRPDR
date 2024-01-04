###############################################################################
## Title: Test pretty_mrn() function
## Project: parseRPDR
## Description: Test pretty_mrn() function
## Copyright: Márton Kolossváry, MD, PhD
## Date: 2023-02-24
###############################################################################

testthat::skip_if_offline()
# Check equality ====================

suppressPackageStartupMessages(library(parseRPDR))

test_that("pretty_mrn if numbers less than needed", {
  expect_equal(pretty_mrn(1, prefix = "MGH", nThread = 1), "MGH:0000001")
  expect_equal(pretty_mrn(1, prefix = "BWH", nThread = 1), "BWH:00000001")
  expect_equal(pretty_mrn(1, prefix = "MCL", nThread = 1), "MCL:000001")
  expect_equal(pretty_mrn(1, prefix = "EMPI", nThread = 1), "EMPI:000000001")
  expect_equal(pretty_mrn(1, prefix = "PMRN", nThread = 1), "PMRN:00000000001")
  expect_equal(pretty_mrn(1, prefix = "ABC", nThread = 1), "ABC:1")
})

test_that("pretty_mrn if numbers less than needed, but unchanged", {
  expect_equal(pretty_mrn(1, prefix = "MGH", id_length = "asis", nThread = 1), "MGH:1")
  expect_equal(pretty_mrn(1, prefix = "BWH", id_length = "asis", nThread = 1), "BWH:1")
  expect_equal(pretty_mrn(1, prefix = "MCL", id_length = "asis", nThread = 1), "MCL:1")
  expect_equal(pretty_mrn(1, prefix = "EMPI", id_length = "asis", nThread = 1), "EMPI:1")
  expect_equal(pretty_mrn(1, prefix = "PMRN", id_length = "asis", nThread = 1), "PMRN:1")
  expect_equal(pretty_mrn(1, prefix = "ABC", id_length = "asis", nThread = 1), "ABC:1")
})

test_that("pretty_mrn if numbers more than needed", {
  expect_equal(pretty_mrn(1234567890123, prefix = "MGH", nThread = 1), "MGH:7890123")
  expect_equal(pretty_mrn(1234567890123, prefix = "BWH", nThread = 1), "BWH:67890123")
  expect_equal(pretty_mrn(1234567890123, prefix = "MCL", nThread = 1), "MCL:890123")
  expect_equal(pretty_mrn(1234567890123, prefix = "EMPI", nThread = 1), "EMPI:567890123")
  expect_equal(pretty_mrn(1234567890123, prefix = "PMRN", nThread = 1), "PMRN:34567890123")
  expect_equal(pretty_mrn(1234567890123, prefix = "ABC", nThread = 1), "ABC:1234567890123")
})

test_that("pretty_mrn if numbers more than needed, but unchanged", {
  expect_equal(pretty_mrn(1234567890123, prefix = "MGH", id_length = "asis", nThread = 1), "MGH:1234567890123")
  expect_equal(pretty_mrn(1234567890123, prefix = "BWH", id_length = "asis", nThread = 1), "BWH:1234567890123")
  expect_equal(pretty_mrn(1234567890123, prefix = "MCL", id_length = "asis", nThread = 1), "MCL:1234567890123")
  expect_equal(pretty_mrn(1234567890123, prefix = "EMPI", id_length = "asis", nThread = 1), "EMPI:1234567890123")
  expect_equal(pretty_mrn(1234567890123, prefix = "PMRN", id_length = "asis", nThread = 1), "PMRN:1234567890123")
  expect_equal(pretty_mrn(1234567890123, prefix = "ABC", id_length = "asis", nThread = 1), "ABC:1234567890123")
})

test_that("pretty_mrn if numbers less than needed, different sep character", {
  expect_equal(pretty_mrn(1, prefix = "MGH", sep = "__", nThread = 1), "MGH__0000001")
  expect_equal(pretty_mrn(1, prefix = "BWH", sep = "__", nThread = 1), "BWH__00000001")
  expect_equal(pretty_mrn(1, prefix = "MCL", sep = "__", nThread = 1), "MCL__000001")
  expect_equal(pretty_mrn(1, prefix = "EMPI", sep = "__", nThread = 1), "EMPI__000000001")
  expect_equal(pretty_mrn(1, prefix = "PMRN", sep = "__", nThread = 1), "PMRN__00000000001")
  expect_equal(pretty_mrn(1, prefix = "ABC", sep = "__", nThread = 1), "ABC__1")
})

test_that("pretty_mrn if numbers less than needed, but unchanged, different sep character", {
  expect_equal(pretty_mrn(1, prefix = "MGH", id_length = "asis", sep = "__", nThread = 1), "MGH__1")
  expect_equal(pretty_mrn(1, prefix = "BWH", id_length = "asis", sep = "__", nThread = 1), "BWH__1")
  expect_equal(pretty_mrn(1, prefix = "MCL", id_length = "asis", sep = "__", nThread = 1), "MCL__1")
  expect_equal(pretty_mrn(1, prefix = "EMPI", id_length = "asis", sep = "__", nThread = 1), "EMPI__1")
  expect_equal(pretty_mrn(1, prefix = "PMRN", id_length = "asis", sep = "__", nThread = 1), "PMRN__1")
  expect_equal(pretty_mrn(1, prefix = "ABC", id_length = "asis", sep = "__", nThread = 1), "ABC__1")
})

test_that("pretty_mrn if numbers more than needed, different sep character", {
  expect_equal(pretty_mrn(1234567890123, prefix = "MGH", sep = "__", nThread = 1), "MGH__7890123")
  expect_equal(pretty_mrn(1234567890123, prefix = "BWH", sep = "__", nThread = 1), "BWH__67890123")
  expect_equal(pretty_mrn(1234567890123, prefix = "MCL", sep = "__", nThread = 1), "MCL__890123")
  expect_equal(pretty_mrn(1234567890123, prefix = "EMPI", sep = "__", nThread = 1), "EMPI__567890123")
  expect_equal(pretty_mrn(1234567890123, prefix = "PMRN", sep = "__", nThread = 1), "PMRN__34567890123")
  expect_equal(pretty_mrn(1234567890123, prefix = "ABC", sep = "__", nThread = 1), "ABC__1234567890123")
})

test_that("pretty_mrn if numbers more than needed, but unchanged, different sep character", {
  expect_equal(pretty_mrn(1234567890123, prefix = "MGH", sep = "__", id_length = "asis", nThread = 1), "MGH__1234567890123")
  expect_equal(pretty_mrn(1234567890123, prefix = "BWH", sep = "__", id_length = "asis", nThread = 1), "BWH__1234567890123")
  expect_equal(pretty_mrn(1234567890123, prefix = "MCL", sep = "__", id_length = "asis", nThread = 1), "MCL__1234567890123")
  expect_equal(pretty_mrn(1234567890123, prefix = "EMPI", sep = "__", id_length = "asis", nThread = 1), "EMPI__1234567890123")
  expect_equal(pretty_mrn(1234567890123, prefix = "PMRN", sep = "__", id_length = "asis", nThread = 1), "PMRN__1234567890123")
  expect_equal(pretty_mrn(1234567890123, prefix = "ABC", sep = "__", id_length = "asis", nThread = 1), "ABC__1234567890123")
})

test_that("pretty_mrn if multiple length prefix", {
  expect_equal(pretty_mrn(rep(1234567890123, 1000), prefix = rep("MGH", 1000), sep = "__", id_length = "asis", nThread = 1), rep("MGH__1234567890123", 1000))
  expect_equal(pretty_mrn(rep(1234567890123, 1000), prefix = rep("BWH", 1000), sep = "__", id_length = "asis", nThread = 2), rep("BWH__1234567890123", 1000))
  expect_equal(pretty_mrn(rep(1234567890123, 1000), prefix = rep("MCL", 1000), sep = "__", id_length = "asis", nThread = 1), rep("MCL__1234567890123", 1000))
  expect_equal(pretty_mrn(rep(1234567890123, 1000), prefix = rep("EMPI", 1000), sep = "__", id_length = "asis", nThread = 2), rep("EMPI__1234567890123", 1000))
  expect_equal(pretty_mrn(rep(1234567890123, 1000), prefix = rep("PMRN", 1000), sep = "__", id_length = "asis", nThread = 1), rep("PMRN__1234567890123", 1000))
  expect_equal(pretty_mrn(rep(1234567890123, 1000), prefix = rep("ABC", 1000), sep = "__", id_length = "asis", nThread = 2), rep("ABC__1234567890123", 1000))
})
