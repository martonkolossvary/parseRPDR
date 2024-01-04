# ###############################################################################
# ## Title: Test load_all_data() function
# ## Project: parseRPDR
# ## Description: Test load_all_data() function
# ## Copyright: Márton Kolossváry, MD, PhD
# ## Date: 2023-02-24
# ###############################################################################

testthat::skip_if_offline()
# Load and check equality ====================

suppressPackageStartupMessages(library(parseRPDR))
folder_wd    <- "/Users/mjk2/Dropbox (Partners HealthCare)/parseRPDR_test/"
folder_raw   <- paste0(folder_wd, "RAW/")
folder_parse <-  paste0(folder_wd, "PARSE/")
OVERWRITE    <- TRUE

# Check sequential vs. parallel loads -----
## Check load_dem_old() ---
expect_true({
  d_p1 <- suppressMessages(load_all_data(folder = paste0(folder_raw, "LEGACY/"), which_data = c("dem", "lab", "enc", "dia", "prc", "med"), nThread = 2,
                                         many_sources = TRUE, load_report = TRUE, format_orig = FALSE, old_dem = TRUE))
  d_p2 <- suppressMessages(load_all_data(folder = paste0(folder_raw, "LEGACY/"), which_data = c("dem", "lab", "enc", "dia", "prc", "med"), nThread = 2,
                                         many_sources = FALSE, load_report = TRUE, format_orig = FALSE, old_dem = TRUE))
  d_p3 <- suppressMessages(load_all_data(folder = paste0(folder_raw, "LEGACY/"), which_data = c("prc"), nThread = 2,
                                         many_sources = FALSE, load_report = TRUE, format_orig = FALSE, old_dem = TRUE))
  d_s1 <- suppressMessages(load_all_data(folder = paste0(folder_raw, "LEGACY/"), which_data = c("enc"), nThread = 2,
                                         many_sources = TRUE, load_report = TRUE, format_orig = FALSE, old_dem = TRUE))
  if(!(is.null(d_p1) | is.null(d_p2) | is.null(d_p3) | is.null(d_s1))) {TRUE}
})

### Check with default values
d_s <- suppressMessages(load_all_data(folder = folder_raw, which_data = c("mrn", "con", "dem", "car", "rdt", "prc"), nThread = 1,
                                      many_sources = TRUE, load_report = TRUE, format_orig = FALSE))
d_p <- suppressMessages(load_all_data(folder = folder_raw, which_data = c("mrn", "con", "dem", "car", "rdt", "prc"), nThread = 2,
                                      many_sources = TRUE, load_report = TRUE, format_orig = FALSE))


test_that("load_all_data run using sequential and parallel loading returns same results", {
  expect_equal(d_s, d_p)
})

### Check with many_sources = FALSE
d_s <- suppressMessages(load_all_data(folder = folder_raw, which_data = c("mrn", "con", "dem", "car", "rdt", "prc"), nThread = 1,
                                      many_sources = FALSE, load_report = FALSE, format_orig = TRUE))
d_p <- suppressMessages(load_all_data(folder = folder_raw, which_data = c("mrn", "con", "dem", "car", "rdt", "prc"), nThread = 2,
                                      many_sources = FALSE, load_report = FALSE, format_orig = TRUE))


test_that("load_all_data run using sequential and parallel loading returns same results", {
  expect_equal(d_s, d_p)
})

### Check with many_sources = TRUE
d_s <- suppressMessages(load_all_data(folder = folder_raw, which_data = c("mrn", "con", "dem", "car", "rdt", "prc"), nThread = 1,
                                      many_sources = TRUE, load_report = TRUE, format_orig = TRUE))
d_p <- suppressMessages(load_all_data(folder = folder_raw, which_data = c("mrn", "con", "dem", "car", "rdt", "prc"), nThread = 2,
                                      many_sources = TRUE, load_report = TRUE, format_orig = TRUE))


test_that("load_all_data run using sequential and parallel loading returns same results", {
  expect_equal(d_s, d_p)
})

