###############################################################################
## Title: Test load_notes() function
## Project: parseRPDR
## Description: Test load_notes() function
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

supp <- c("car", "dis", "end", "hnp", "opn", "pat", "prg", "pul", "rad", "vis")
d_fname <- list.files(folder_raw, full.names = TRUE)[grep(paste0(paste(supp, collapse = ".txt|"), ".txt"), list.files(folder_raw), ignore.case = TRUE)]

expect_error(load_notes(d_fname[i], type = "TEST", nThread = 1, load_report = FALSE))
expect_error(load_notes("TEST", type = supp[i], nThread = 1, load_report = FALSE))

for(i in 1) {
  ## Check sequential vs. parallel loads -----
  ### Run without returing the notes
  d_s <- suppressMessages(load_notes(d_fname[i], type = supp[i], nThread = 1, load_report = FALSE))
  d_p <- suppressMessages(load_notes(d_fname[i], type = supp[i], nThread = 2, load_report = FALSE))


  test_that(paste0("load_notes for ", supp[i], " run using sequential and parallel loading returns same results without report"), {
    expect_equal(d_s, d_p)
  })

  ### Run with returning the notes
  d_s <- suppressMessages(load_notes(d_fname[i], type = supp[i], nThread = 1))
  d_p <- suppressMessages(load_notes(d_fname[i], type = supp[i], nThread = 2))


  test_that(paste0("load_notes for ", supp[i], " run using sequential and parallel loading returns same results with report"), {
    expect_equal(d_s, d_p)
  })

  # Run with returning the notes in original format
  d_s <- suppressMessages(load_notes(d_fname[i], type = supp[i], nThread = 1, format_orig = TRUE))
  d_p <- suppressMessages(load_notes(d_fname[i], type = supp[i], nThread = 2, format_orig = TRUE))


  test_that(paste0("load_notes for ", supp[i], " run using sequential and parallel loading returns same results with original report"), {
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

  if(OVERWRITE | !file.exists(paste0(folder_parse, supp[i], ".csv"))) {data.table::fwrite(d_s, paste0(folder_parse, supp[i], ".csv"))}
  l_s <- data.table::fread(paste0(folder_parse, supp[i], ".csv"), na.strings = "NA", strip.white = FALSE, colClasses = apply(d_s, 2, class))


  test_that("Compare loaded data with legacy data", {
    expect_equal(l_s, d_s)
  })
}

