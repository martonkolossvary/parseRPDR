###############################################################################
## Title: Test load_all_ids_mi2b2() function
## Project: parseRPDR
## Description: Test load_all_ids_mi2b2() function
## Copyright: Márton Kolossváry, MD, PhD
## Date: 2023-02-24
###############################################################################

# Load and check equality ====================

testthat::skip_if_offline()
suppressPackageStartupMessages(library(parseRPDR))
folder_wd    <- "/Users/mjk2/Dropbox (Partners HealthCare)/parseRPDR_test/"
folder_raw   <- paste0(folder_wd, "RAW/")
folder_parse <-  paste0(folder_wd, "PARSE/")
OVERWRITE    <- TRUE

## Check sequential vs. parallel loads -----
d_fname <- list.files(folder_raw, full.names = TRUE)[grep("mrn.txt", list.files(folder_raw), ignore.case = TRUE)]
d_mrn <- suppressMessages(load_mrn(d_fname, nThread = 1))
d_fname <- list.files(folder_raw, full.names = TRUE)[grep("con.txt", list.files(folder_raw), ignore.case = TRUE)]
d_con <- suppressMessages(load_con(d_fname, nThread = 2))


expect_true({
  c_mgh <- suppressMessages(all_ids_mi2b2(type = "MGH", d_mrn = d_mrn, d_con = d_con))
  c_bwh <- suppressMessages(all_ids_mi2b2(type = "BWH", d_mrn = d_mrn, d_con = d_con))
  if(!is.null(c_mgh) & !is.null(c_bwh)) TRUE
})

## Compare loaded data with legacy data -----
### Convert dates to text and remove NAs
if(OVERWRITE | !file.exists(paste0(folder_parse, "all_ids_mi2b2_MGH.csv"))) {write.table(c_mgh, paste0(folder_parse, "all_ids_mi2b2_MGH.csv"), na = "NA", row.names = FALSE, sep = ",")}
l_s <- read.table(paste0(folder_parse, "all_ids_mi2b2_MGH.csv"), sep = ",", na.strings = "NA", header = TRUE)


test_that("Compare loaded data with legacy data", {
  expect_equal(l_s[[1]], c_mgh)
})

### Convert dates to text and remove NAs
if(OVERWRITE | !file.exists(paste0(folder_parse, "all_ids_mi2b2_BWH.csv"))) {write.table(c_bwh, paste0(folder_parse, "all_ids_mi2b2_BWH.csv"), na = "NA", row.names = FALSE, sep = ",")}
l_s <- read.table(paste0(folder_parse, "all_ids_mi2b2_BWH.csv"), sep = ",", na.strings = "NA", header = TRUE)


test_that("Compare loaded data with legacy data", {
  expect_equal(l_s[[1]], c_bwh)
})
