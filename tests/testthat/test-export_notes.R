###############################################################################
## Title: Test export_notes() function
## Project: parseRPDR
## Description: Test export_notes() function
## Copyright: Márton Kolossváry, MD, PhD
## Date: 2023-03-31
###############################################################################

testthat::skip_if_offline()
# Load and check equality ====================

suppressPackageStartupMessages(library(parseRPDR))
folder_wd    <- "/Users/mjk2/Dropbox (Partners HealthCare)/parseRPDR_test/"
folder_raw   <- paste0(folder_wd, "RAW/")
folder_parse <-  paste0(folder_wd, "PARSE/")

supp <- c("car", "dis", "end", "hnp", "opn", "pat", "prg", "pul", "rad", "vis")
d_fname <- list.files(folder_raw, full.names = TRUE)[grep(paste0(paste(supp, collapse = ".txt|"), ".txt"), list.files(folder_raw), ignore.case = TRUE)]

i = 1
name1 = "ID_MERGE"
name2 = "car_rep_num"
code =  "car_rep_txt"
d_p <- suppressMessages(load_notes(d_fname[i], type = supp[i], nThread = 2, format_orig = TRUE))

export_notes(d_p[1, ], folder = folder_parse, code = code, name2 = name2)
expect_true({
  file.exists(paste0(folder_parse, d_p[i, ][[name1]], "_", d_p[i, ][[name2]], ".txt"))
})
