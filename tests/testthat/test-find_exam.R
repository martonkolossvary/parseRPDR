###############################################################################
## Title: Test find_exam() function
## Project: parseRPDR
## Description: Test find_exam() function
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

## Check sequential vs. parallel loads -----
d_fname <- list.files(folder_raw, full.names = TRUE)[grep("enc.txt", list.files(folder_raw), ignore.case = TRUE)]
data_enc_ED <- suppressMessages(load_enc(d_fname, nThread = 2))
data_enc_ED <- data_enc_ED[enc_clinic == "MGH EMERGENCY (10020010608)"]
data_enc_ED <- data_enc_ED[!duplicated(data_enc_ED$ID_MERGE)]

d_fname <- list.files(folder_raw, full.names = TRUE)[grep("rdt.txt", list.files(folder_raw), ignore.case = TRUE)]
data_rdt <- suppressMessages(load_rdt(d_fname, nThread = 2))


expect_true({
  c_s <- suppressMessages(find_exam(d_from = data_rdt, d_to = data.table::rbindlist(list(data_enc_ED, data_enc_ED, data_enc_ED, data_enc_ED, data_enc_ED, data_enc_ED, data_enc_ED, data_enc_ED, data_enc_ED, data_enc_ED)),
                                    d_from_ID = "ID_MERGE", d_to_ID = "ID_MERGE",
                                    d_from_time = "time_rdt", d_to_time = "time_enc_admit",
                                    time_diff_name = "time_diff_ED_rdt", before = FALSE, after = FALSE, add_column = "enc_enc_numb",
                                    time = 3, time_unit = "days", multiple = "closest", nThread = 2, shared_RAM = FALSE))

  c_s <- suppressMessages(find_exam(d_from = data_rdt, d_to = data.table::rbindlist(list(data_enc_ED, data_enc_ED, data_enc_ED, data_enc_ED, data_enc_ED, data_enc_ED, data_enc_ED, data_enc_ED, data_enc_ED, data_enc_ED)),
                                    d_from_ID = "ID_MERGE", d_to_ID = "ID_MERGE",
                                    d_from_time = "time_rdt", d_to_time = "time_enc_admit",
                                    time_diff_name = "time_diff_ED_rdt", before = FALSE, after = TRUE, add_column = "enc_enc_numb",
                                    time = 3, time_unit = "days", multiple = "closest", nThread = 2, shared_RAM = FALSE))

  c_s <- suppressMessages(find_exam(d_from = data_rdt, d_to = data.table::rbindlist(list(data_enc_ED, data_enc_ED)),
                                    d_from_ID = "ID_MERGE", d_to_ID = "ID_MERGE",
                                    d_from_time = "time_rdt", d_to_time = "time_enc_admit",
                                    time_diff_name = "time_diff_ED_rdt", before = TRUE, after = FALSE, add_column = "enc_enc_numb", keep_data = TRUE,
                                    time = 3, time_unit = "days", multiple = "earliest", nThread = 2, shared_RAM = FALSE))

  c_s <- suppressMessages(find_exam(d_from = data_rdt, d_to = data.table::rbindlist(list(data_enc_ED, data_enc_ED, data_enc_ED, data_enc_ED, data_enc_ED, data_enc_ED, data_enc_ED, data_enc_ED, data_enc_ED, data_enc_ED)),
                                    d_from_ID = "ID_MERGE", d_to_ID = "ID_MERGE",
                                    d_from_time = "time_rdt", d_to_time = "time_enc_admit",
                                    time_diff_name = "time_diff_ED_rdt", before = FALSE, after = FALSE, add_column = "enc_enc_numb", keep_data = TRUE,
                                    time = 3, time_unit = "days", multiple = "closest", nThread = 2, shared_RAM = TRUE))

  c_s <- suppressMessages(find_exam(d_from = data_rdt[1:100, ], d_to = data_enc_ED,
                                    d_from_ID = "ID_MERGE", d_to_ID = "ID_MERGE",
                                    d_from_time = "time_rdt", d_to_time = "time_enc_admit",
                                    time_diff_name = "time_diff_ED_rdt", before = FALSE, after = TRUE, add_column = "enc_enc_numb", keep_data = TRUE,
                                    time = 3, time_unit = "days", multiple = "earliest", nThread = 2, shared_RAM = TRUE))

  d_test_from = data_rdt[c(1:10, 63), ]
  d_test_from[ID_MERGE == "100239955"]$time_rdt <- NA
  d_test_to <- data_enc_ED[1:5, ]
  d_test_to[ID_MERGE == "100239955", ]$time_enc_admit <- NA
  d_test_to$enc_enc_numb <- NA
  c_s <- suppressMessages(find_exam(d_from = d_test_from, d_to = d_test_to,
                                    d_from_ID = "ID_MERGE", d_to_ID = "ID_MERGE",
                                    d_from_time = "time_rdt", d_to_time = "time_enc_admit",
                                    time_diff_name = "time_diff_ED_rdt", before = FALSE, after = TRUE, add_column = "enc_enc_numb", keep_data = TRUE,
                                    time = 3, time_unit = "days", multiple = "earliest", nThread = 2, shared_RAM = TRUE))
  if(!is.null(c_s)) TRUE
})

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

d_s_sum <- suppressMessages(find_exam(d_from = data_rdt, d_to = data_enc_ED,
                                      d_from_ID = "ID_MERGE", d_to_ID = "ID_MERGE",
                                      d_from_time = "time_rdt", d_to_time = "time_enc_admit",
                                      time_diff_name = "time_diff_ED_rdt", before = TRUE, after = TRUE,
                                      time = 3, time_unit = "days", multiple = "all", nThread = 2, shared_RAM = TRUE))
d_p_sum <- suppressMessages(find_exam(d_from = data_rdt, d_to = data_enc_ED,
                                      d_from_ID = "ID_MERGE", d_to_ID = "ID_MERGE",
                                      d_from_time = "time_rdt", d_to_time = "time_enc_admit",
                                      time_diff_name = "time_diff_ED_rdt", before = TRUE, after = TRUE,
                                      time = 3, time_unit = "days", multiple = "all", nThread = 2, shared_RAM = TRUE))


test_that("find_exam run using sequential and parallel loading returns same results", {
  expect_equal(d_s_sum, d_p_sum) #bm only runs in nThread>1
})

test_that("find_exam run using big memory loading returns same results", {
  expect_equal(d_s, d_p_sum) #bm only runs in nThread>1
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

if(OVERWRITE | !file.exists(paste0(folder_parse, "find_exam.csv"))) {data.table::fwrite(d_s, paste0(folder_parse, "find_exam.csv"))}
l_s <- data.table::fread(paste0(folder_parse, "find_exam.csv"), na.strings = "NA", strip.white = FALSE, colClasses = apply(d_s, 2, class))


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

if(OVERWRITE | !file.exists(paste0(folder_parse, "find_exam_sum.csv"))) {data.table::fwrite(d_s_sum, paste0(folder_parse, "find_exam_sum.csv"))}
l_s <- data.table::fread(paste0(folder_parse, "find_exam_sum.csv"), na.strings = "NA", strip.white = FALSE, colClasses = apply(d_s_sum, 2, class))


test_that("Compare loaded data with legacy data", {
  expect_equal(l_s, d_s_sum)
})

