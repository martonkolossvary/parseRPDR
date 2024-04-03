###############################################################################
## Title: Test convert_notes() function
## Project: parseRPDR
## Description: Test convert_notes() function
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
d_fname <- list.files(folder_raw, full.names = TRUE)[grep("rad.txt", list.files(folder_raw), ignore.case = TRUE)]
d_p <- suppressMessages(load_notes(d_fname, type = "rad", nThread = 2))
anchors = c("Exam Code", "Ordering Provider", "HISTORY", "Associated Reports", "Report Below", "REASON", "REPORT",
            "TECHNIQUE", "COMPARISON", "FINDINGS", "IMPRESSION", "RECOMMENDATION", "SIGNATURES", "report_end")

expect_true({
  c_s <- suppressMessages(convert_notes(d_p, code = "rad_rep_txt", anchors = anchors, nThread = 2))
  if(!is.null(c_s)) TRUE
})

c_s <- suppressMessages(convert_notes(d_p, code = "rad_rep_txt", anchors = anchors, nThread = 1))
c_p <- suppressMessages(convert_notes(d_p, code = "rad_rep_txt", anchors = anchors, nThread = 2))


test_that("convert_dia run using sequential and parallel loading returns same results", {
  expect_equal(c_s, c_p)
})


## Compare loaded data with legacy data -----
expect_true({
  date_cols <- colnames(c_s)[which(as.vector(c_s[,lapply(.SD, class)][1,]) == "POSIXct")]
  suppressWarnings(c_s[,(date_cols):= lapply(.SD, as.character), .SDcols = date_cols])
  c_s[is.na(c_s)] <- ""
  #c_s[] <- lapply(c_s, gsub, pattern = '"', replacement = '')
  for (j in colnames(c_s)) data.table::set(c_s, j = j, value = gsub(pattern = '"', replacement = '', c_s[[j]], fixed = TRUE))
  for (j in colnames(c_s)) data.table::set(c_s, j = j, value = gsub(pattern = '\\', replacement = '', c_s[[j]], fixed = TRUE))
  for (j in colnames(c_s)) data.table::set(c_s, j = j, value = gsub(pattern = '\\\\', replacement = '', c_s[[j]], fixed = TRUE))
  TRUE
})
if(OVERWRITE | !file.exists(paste0(folder_parse, "notes_conv.csv"))) {data.table::fwrite(c_s, paste0(folder_parse, "notes_conv.csv"))}
l_s <- data.table::fread(paste0(folder_parse, "notes_conv.csv"), na.strings = "NA", strip.white = FALSE, colClasses = apply(c_s, 2, class))


test_that("Compare loaded data with legacy data", {
  expect_equal(l_s, c_s)
})

