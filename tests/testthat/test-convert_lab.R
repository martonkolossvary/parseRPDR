###############################################################################
## Title: Test convert_lab() function
## Project: parseRPDR
## Description: Test convert_lab() function
## Copyright: Márton Kolossváry, MD, PhD
## Date: 2023-02-24
###############################################################################

testthat::skip_on_cran()
# Load and check equality ====================

suppressPackageStartupMessages(library(parseRPDR))
folder_wd    <- "/Users/mjk2/Dropbox (Partners HealthCare)/parseRPDR_test/"
folder_raw   <- paste0(folder_wd, "RAW/")
folder_parse <-  paste0(folder_wd, "PARSE/")
OVERWRITE    <- TRUE


## Test equality -----
d_fname <- list.files(folder_raw, full.names = TRUE)[grep("lab.txt", list.files(folder_raw), ignore.case = TRUE)]
d_p <- suppressMessages(load_lab(d_fname, nThread = 2))
diseases <- list(HT = c("ICD10:I10"), Stroke = c("ICD9:434.91", "ICD9:I63.50"))

expect_true({
  c_s <- suppressMessages(convert_lab(d_p))
  if(!is.null(c_s)) TRUE
})


## Compare loaded data with legacy data -----
expect_true({
  date_cols <- colnames(c_s)[which(as.vector(c_s[,lapply(.SD, class)])[1,] == "POSIXct")]
  suppressWarnings(c_s[,(date_cols):= lapply(.SD, as.character), .SDcols = date_cols])
  c_s[is.na(c_s)] <- ""
  #c_s[] <- lapply(c_s, gsub, pattern = '"', replacement = '')
  for (j in colnames(c_s)) data.table::set(c_s, j = j, value = gsub(pattern = '"', replacement = '', c_s[[j]], fixed = TRUE))
  for (j in colnames(c_s)) data.table::set(c_s, j = j, value = gsub(pattern = '\\', replacement = '', c_s[[j]], fixed = TRUE))
  for (j in colnames(c_s)) data.table::set(c_s, j = j, value = gsub(pattern = '\\\\', replacement = '', c_s[[j]], fixed = TRUE))
  TRUE
})
if(OVERWRITE | !file.exists(paste0(folder_parse, "lab_conv.csv"))) {data.table::fwrite(c_s, paste0(folder_parse, "lab_conv.csv"))}
l_s <- data.table::fread(paste0(folder_parse, "lab_conv.csv"), na.strings = "NA", strip.white = FALSE, colClasses = apply(c_s, 2, class))


test_that("Compare loaded data with legacy data", {
  expect_equal(l_s, c_s)
})

