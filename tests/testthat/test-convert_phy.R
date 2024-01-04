###############################################################################
## Title: Test convert_phy() function
## Project: parseRPDR
## Description: Test convert_phy() function
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


## Test equality -----
d_fname <- list.files(folder_raw, full.names = TRUE)[grep("phy.txt", list.files(folder_raw), ignore.case = TRUE)]
d_p <- suppressMessages(load_phy(d_fname, nThread = 2))
anthropometrics <- list(Weight = c("LMR:3688", "EPIC:WGT"), Height = c("LMR:3771", "EPIC:HGT"))

expect_true({
  c_s <- suppressMessages(convert_phy(d_p, codes_to_find = anthropometrics, nThread = 4))
  if(!is.null(c_s)) TRUE
})

c_s <- suppressMessages(convert_phy(d_p, codes_to_find = anthropometrics, nThread = 1))
c_p <- suppressMessages(convert_phy(d_p, codes_to_find = anthropometrics, nThread = 2))


test_that("convert_phy run using sequential and parallel loading returns same results with original columns", {
  expect_equal(c_s, c_p)
})

c_s <- suppressMessages(convert_phy(d_p, codes_to_find = anthropometrics, nThread = 1))
c_p <- suppressMessages(convert_phy(d_p, codes_to_find = anthropometrics, nThread = 2))


test_that("convert_phy run using sequential and parallel loading returns same results", {
  expect_equal(c_s, c_p)
})

expect_true({
  c_p_sum <- suppressMessages(convert_phy(d_p, codes_to_find = anthropometrics, collapse = "ID_MERGE", aggr_type = "earliest", nThread = 2))
  c_p_sum <- suppressMessages(convert_phy(d_p, codes_to_find = NULL, collapse = "ID_MERGE", aggr_type = "earliest", nThread = 2))
  TRUE
})

c_s_sum <- suppressMessages(convert_phy(d_p, codes_to_find = anthropometrics, collapse = "ID_MERGE", aggr_type = "latest", nThread = 1))
c_p_sum <- suppressMessages(convert_phy(d_p, codes_to_find = anthropometrics, collapse = "ID_MERGE", aggr_type = "latest", nThread = 2))


test_that("convert_phy summarizing using sequential and parallel loading returns same results", {
  expect_equal(c_s_sum, c_p_sum)
})

c_s_sum <- suppressMessages(convert_phy(d_p, codes_to_find = anthropometrics, collapse = "ID_MERGE", aggr_type = "earliest", nThread = 1))
c_p_sum <- suppressMessages(convert_phy(d_p, codes_to_find = anthropometrics, collapse = "ID_MERGE", aggr_type = "earliest", nThread = 2))


test_that("convert_phy summarizing using sequential and parallel loading returns same results", {
  expect_equal(c_s_sum, c_p_sum)
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
if(OVERWRITE | !file.exists(paste0(folder_parse, "phy_conv.csv"))) {data.table::fwrite(c_s, paste0(folder_parse, "phy_conv.csv"))}
l_s <- data.table::fread(paste0(folder_parse, "phy_conv.csv"), na.strings = "NA", strip.white = FALSE, colClasses = apply(c_s, 2, class))


test_that("Compare loaded data with legacy data", {
  expect_equal(l_s, c_s)
})

expect_true({
  date_cols <- colnames(c_s_sum)[which(as.vector(c_s_sum[,lapply(.SD, class)])[1,] == "POSIXct")]
  suppressWarnings(c_s_sum[,(date_cols):= lapply(.SD, as.character), .SDcols = date_cols])
  c_s_sum[is.na(c_s_sum)] <- ""
  #c_s_sum[] <- lapply(c_s_sum, gsub, pattern = '"', replacement = '')
  for (j in colnames(c_s_sum)) data.table::set(c_s_sum, j = j, value = gsub(pattern = '"', replacement = '', c_s_sum[[j]], fixed = TRUE))
  for (j in colnames(c_s_sum)) data.table::set(c_s_sum, j = j, value = gsub(pattern = '\\', replacement = '', c_s_sum[[j]], fixed = TRUE))
  for (j in colnames(c_s_sum)) data.table::set(c_s_sum, j = j, value = gsub(pattern = '\\\\', replacement = '', c_s_sum[[j]], fixed = TRUE))
  TRUE
})
if(OVERWRITE | !file.exists(paste0(folder_parse, "phy_conv_sum.csv"))) {data.table::fwrite(c_s_sum, paste0(folder_parse, "phy_conv_sum.csv"))}
l_s_sum <- data.table::fread(paste0(folder_parse, "phy_conv_sum.csv"), na.strings = "NA", strip.white = FALSE, colClasses = apply(c_s_sum, 2, class))


test_that("Compare loaded data with legacy data", {
  expect_equal(l_s_sum, c_s_sum)
})

