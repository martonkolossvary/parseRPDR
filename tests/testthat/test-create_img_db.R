###############################################################################
## Title: Test create_img_db() function
## Project: parseRPDR
## Description: Test create_img_db() function
## Copyright: Márton Kolossváry, MD, PhD
## Date: 2023-02-24
###############################################################################

testthat::skip_if_offline()
# Load and check equality ====================

suppressPackageStartupMessages(library(parseRPDR))
folder_wd    <- "/Users/mjk2/Dropbox (Partners HealthCare)/parseRPDR_test/"
folder_raw   <- paste0(folder_wd, "DICOM/")
folder_parse <-  paste0(folder_wd, "PARSE/")
OVERWRITE    <- TRUE

## Check sequential vs. parallel loads -----
d_fname <- folder_raw
expect_true({
  c_s <- suppressMessages(create_img_db(path = d_fname, nThread = 1, ext = ".something"))
  c_s <- suppressMessages(create_img_db(path = d_fname, nThread = 1, ext = ""))
  if(!is.null(c_s)) TRUE
})

d_s <- suppressMessages(create_img_db(path = d_fname, nThread = 1))
d_p <- suppressMessages(create_img_db(path = d_fname, nThread = 2))


test_that("create_img_db run using sequential and parallel loading returns same results", {
  expect_equal(d_s, d_p)
})

d_s_sum <- suppressMessages(create_img_db(path = d_fname, nThread = 1, all = FALSE))
d_p_sum <- suppressMessages(create_img_db(path = d_fname, nThread = 2, all = FALSE))


test_that("create_img_db run using sequential and parallel loading returns same results without all files", {
  expect_equal(d_s_sum, d_p_sum)
})


## Compare loaded data with legacy data -----
### Convert dates to text and remove NAs
expect_true({
  date_cols <- colnames(d_s)[which(as.vector(d_s[,lapply(.SD, class)])[1,] == "POSIXct")]
  suppressWarnings(d_s[,(date_cols):= lapply(.SD, as.character), .SDcols = date_cols])
  d_s[is.na(d_s)] <- ""
  #d_s[] <- lapply(d_s, gsub, pattern = '"', replacement = '')
  for (j in colnames(d_s)) data.table::set(d_s, j = j, value = gsub(pattern = '"', replacement = '', d_s[[j]], fixed = TRUE))
  for (j in colnames(d_s)) data.table::set(d_s, j = j, value = gsub(pattern = '\\', replacement = '', d_s[[j]], fixed = TRUE))
  for (j in colnames(d_s)) data.table::set(d_s, j = j, value = gsub(pattern = '\\\\', replacement = '', d_s[[j]], fixed = TRUE))
  TRUE
})

if(OVERWRITE | !file.exists(paste0(folder_parse, "create_img_db.csv"))) {data.table::fwrite(d_s, paste0(folder_parse, "create_img_db.csv"))}
l_s <- data.table::fread(paste0(folder_parse, "create_img_db.csv"), na.strings = "NA", strip.white = FALSE, colClasses = apply(d_s, 2, class))


test_that("Compare loaded data with legacy data", {
  expect_equal(l_s, d_s)
})


### Convert dates to text and remove NAs
expect_true({
  date_cols <- colnames(d_s_sum)[which(as.vector(d_s_sum[,lapply(.SD, class)])[1,] == "POSIXct")]
  suppressWarnings(d_s_sum[,(date_cols):= lapply(.SD, as.character), .SDcols = date_cols])
  d_s_sum[is.na(d_s_sum)] <- ""
  #d_s_sum[] <- lapply(d_s_sum, gsub, pattern = '"', replacement = '')
  for (j in colnames(d_s_sum)) data.table::set(d_s_sum, j = j, value = gsub(pattern = '"', replacement = '', d_s_sum[[j]], fixed = TRUE))
  for (j in colnames(d_s_sum)) data.table::set(d_s_sum, j = j, value = gsub(pattern = '\\', replacement = '', d_s_sum[[j]], fixed = TRUE))
  for (j in colnames(d_s_sum)) data.table::set(d_s_sum, j = j, value = gsub(pattern = '\\\\', replacement = '', d_s_sum[[j]], fixed = TRUE))
  TRUE
})

if(OVERWRITE | !file.exists(paste0(folder_parse, "create_img_db_sum.csv"))) {data.table::fwrite(d_s_sum, paste0(folder_parse, "create_img_db_sum.csv"))}
l_s <- data.table::fread(paste0(folder_parse, "create_img_db_sum.csv"), na.strings = "NA", strip.white = FALSE, colClasses = apply(d_s_sum, 2, class))


test_that("Compare loaded data with legacy data", {
  expect_equal(l_s, d_s_sum)
})

