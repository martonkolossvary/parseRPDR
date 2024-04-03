###############################################################################
## Title: Test find_exam() function
## Project: parseRPDR
## Description: Test find_exam() function
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
d_fname <- list.files(folder_raw, full.names = TRUE)[grep("enc.txt", list.files(folder_raw), ignore.case = TRUE)]
data_enc_ED <- suppressMessages(load_enc(d_fname, nThread = 2))
data_enc_ED <- data_enc_ED[enc_clinic == "MGH EMERGENCY (10020010608)" | enc_clinic == "MGH EMERGENCY" |
                             enc_clinic == "MGH Emergency Associates (70)" | enc_clinic == "BWF EMERGENCY FLK (10040010073)"]

d_fname <- list.files(folder_raw, full.names = TRUE)[grep("rdt.txt", list.files(folder_raw), ignore.case = TRUE)]
data_rdt <- suppressMessages(load_rdt(d_fname, nThread = 2))


expect_true({
  c_s <- suppressMessages(find_exam(d_from = data_rdt, d_to = data.table::rbindlist(list(data_enc_ED, data_enc_ED, data_enc_ED)),
                                    d_from_ID = "ID_MERGE", d_to_ID = "ID_MERGE",
                                    d_from_time = "time_rdt", d_to_time = "time_enc_admit",
                                    time_diff_name = "time_diff_ED_rdt", before = FALSE, after = FALSE, add_column = "enc_enc_numb",
                                    time = 3, time_unit = "days", multiple = "closest", nThread = 2))

  c_s <- suppressMessages(find_exam(d_from = data_rdt, d_to = data.table::rbindlist(list(data_enc_ED, data_enc_ED, data_enc_ED)),
                                    d_from_ID = "ID_MERGE", d_to_ID = "ID_MERGE",
                                    d_from_time = "time_rdt", d_to_time = "time_enc_admit",
                                    time_diff_name = "time_diff_ED_rdt", before = FALSE, after = TRUE, add_column = "enc_enc_numb",
                                    time = 3, time_unit = "days", multiple = "closest", nThread = 2))

  c_s <- suppressMessages(find_exam(d_from = data_rdt, d_to = data.table::rbindlist(list(data_enc_ED, data_enc_ED)),
                                    d_from_ID = "ID_MERGE", d_to_ID = "ID_MERGE",
                                    d_from_time = "time_rdt", d_to_time = "time_enc_admit",
                                    time_diff_name = "time_diff_ED_rdt", before = TRUE, after = FALSE, add_column = "enc_enc_numb", keep_data = TRUE,
                                    time = 3, time_unit = "days", multiple = "earliest", nThread = 2))

  if(!is.null(c_s)) TRUE
})

data_enc_ED <- data_enc_ED[enc_clinic == "MGH EMERGENCY (10020010608)"]

d_s <- suppressMessages(find_exam(d_from = data_rdt, d_to = data_enc_ED,
                                  d_from_ID = "ID_MERGE", d_to_ID = "ID_MERGE",
                                  d_from_time = "time_rdt", d_to_time = "time_enc_admit",
                                  time_diff_name = "time_diff_ED_rdt", before = TRUE, after = TRUE,
                                  time = 3, time_unit = "days", multiple = "all", nThread = 1, shared_RAM = FALSE))
d_p <- suppressMessages(find_exam(d_from = data_rdt, d_to = data_enc_ED,
                                  d_from_ID = "ID_MERGE", d_to_ID = "ID_MERGE",
                                  d_from_time = "time_rdt", d_to_time = "time_enc_admit",
                                  time_diff_name = "time_diff_ED_rdt", before = TRUE, after = TRUE,
                                  time = 3, time_unit = "days", multiple = "all", nThread = 2, shared_RAM = FALSE))


test_that("find_exam run using sequential and parallel loading returns same results", {
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

if(OVERWRITE | !file.exists(paste0(folder_parse, "find_exam.csv"))) {data.table::fwrite(d_s, paste0(folder_parse, "find_exam.csv"))}
l_s <- data.table::fread(paste0(folder_parse, "find_exam.csv"), na.strings = "NA", strip.white = FALSE, colClasses = apply(d_s, 2, class))


test_that("Compare loaded data with legacy data", {
  expect_equal(l_s, d_s)
})
