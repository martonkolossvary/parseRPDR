###############################################################################
## Title: Test load_dia() function
## Project: parseRPDR
## Description: Test load_dia() function
## Copyright: Márton Kolossváry, MD, PhD
## Date: 2023-02-24
###############################################################################

testthat::skip_if_offline()
# Load and check equality ====================

suppressPackageStartupMessages(library(parseRPDR))
folder_wd    <- "/Users/mjk2/Dropbox (Partners HealthCare)/parseRPDR_test/"
folder_raw   <- paste0(folder_wd, "RAW/")
folder_parse <-  paste0(folder_wd, "PARSE/")
OVERWRITE    <- TRUE

## Check sequential vs. parallel loads -----
d_fname <- list.files(folder_raw, full.names = TRUE)[grep("dia.txt", list.files(folder_raw), ignore.case = TRUE)]
d_s <- suppressMessages(load_dia(d_fname, nThread = 1))
d_p <- suppressMessages(load_dia(d_fname, nThread = 2))


test_that("load_dia run using sequential and parallel loading returns same results", {
  expect_equal(d_s, d_p)
})


## Compare loaded data with legacy data -----
### Convert dates to text and remove NAs
expect_true({
  date_cols <- colnames(d_s)[which(as.vector(d_s[,lapply(.SD, class)][1,]) == "POSIXct")]
  suppressWarnings(d_s[,(date_cols):= lapply(.SD, as.character), .SDcols = date_cols])
  d_s[is.na(d_s)] <- ""
  #d_s[] <- lapply(d_s, gsub, pattern = '"', replacement = '')
  for (j in colnames(d_s)) data.table::set(d_s, j = j, value = gsub(pattern = '"', replacement = '', d_s[[j]], fixed = TRUE))
  for (j in colnames(d_s)) data.table::set(d_s, j = j, value = gsub(pattern = '\\', replacement = '', d_s[[j]], fixed = TRUE))
  for (j in colnames(d_s)) data.table::set(d_s, j = j, value = gsub(pattern = '\\\\', replacement = '', d_s[[j]], fixed = TRUE))
  TRUE
})

if(OVERWRITE | !file.exists(paste0(folder_parse, "dia.csv"))) {data.table::fwrite(d_s, paste0(folder_parse, "dia.csv"))}
l_s <- data.table::fread(paste0(folder_parse, "dia.csv"), na.strings = "NA", strip.white = FALSE, colClasses = apply(d_s, 2, class))


test_that("Compare loaded data with legacy data", {
  expect_equal(l_s, d_s)
})

