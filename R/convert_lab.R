#' @title Converts lab results to normal/abnormal based-on reference values.
#' @export
#'
#' @description Analyzes laboratory data loaded using \emph{load_lab}. Converts laboratory results to values without ">" or "<" by creating a column where these characters are removed.
#' Furthermore, adds two indicator columns where based-on the reference ranges or the Abnormal_Flag column in RPDR (lab_result_abn using load_lab), the value is considered normal or abnormal.
#'
#' @param d data.table, database containing laboratory results data loaded using the \emph{load_lab} function.
#' @param code_results string vector, column name containing the results. Defaults to: \emph{"lab_result"}.
#' @param code_reference  string vector, column name containing the reference ranges. Defaults to: \emph{"lab_result_range"}.
#' @param code_flag, string vector, column name containing the abnormal flags. Defaults to: \emph{"lab_result_abn"}.
#'
#' @return data.table, with three additional columns: \emph{"lab_result_pretty"} containing numerical results. In case of ">" or "<" notation,
#' the numeric value is returned, as we only have information that it is at least as much or not larger than a given value.
#' The other column: \emph{"lab_result_abn_pretty"} can take values: NORMAL/ABNORMAL, depending on whether the value is within the reference range.
#' Please be aware that there can be very different representations of values, and in some cases this will result in misclassification of values.
#' The third column: \emph{"lab_result_abn_flag_pretty"} gives abnormal if the original Abnormal_Flag column contains any information.
#' Borderline values are considered NORMAL.
#'
#' @encoding UTF-8
#'
#' @examples \dontrun{
#' #Convert loaded lab results
#' data_lab_pretty <- convert_lab(d = data_lab)
#' data_lab_pretty[, c("lab_result", "lab_result_pretty", "lab_result_range",
#' "lab_result_abn_pretty", "lab_result_abn_flag_pretty")]
#' }

convert_lab <- function(d, code_results = "lab_result", code_reference = "lab_result_range", code_flag = "lab_result_abn") {

  message(paste0("Converting lab results."))
  #Parse lab result values
  d_res <- d[[code_results]]
  d_res_pretty <- gsub("[^0-9.-]", "", d_res)

  #If range present in value then consider largest value
  d_res_pretty[grep("-", x = d_res_pretty, ignore.case = TRUE)] <- gsub(".*-", "",
                                                                        x = d_res_pretty[d_res_pretty[grep("-", x = d_res_pretty, ignore.case = TRUE)]],
                                                                        fixed = FALSE)

  #Change negative to neg
  d_res_pretty[grep("negative", x = d_res, ignore.case = TRUE)] <- "NEG"
  d_res_pretty[grep("positive", x = d_res, ignore.case = TRUE)] <- "POS"
  d_res_pretty[grep("borderline", x = d_res, ignore.case = TRUE)] <- "BORD"

  #Change refused results to NA
  d_res_pretty[grep("refused", x = d_res, ignore.case = TRUE)]   <- NA
  d_res_pretty[grep("cancelled", x = d_res, ignore.case = TRUE)] <- NA
  d_res_pretty[grep("credit", x = d_res, ignore.case = TRUE)]    <- NA
  d_res_pretty[grep("pend", x = d_res, ignore.case = TRUE)]      <- NA
  d_res_pretty[grep("SAMPLE HEMOLYZED, TO BE REDRAWN", x = d_res, ignore.case = TRUE)]      <- NA
  d_res_pretty[grep("REQUEST CREDITED", x = d_res, ignore.case = TRUE)]                     <- NA
  d_res_pretty[grep("No purple top tube received", x = d_res, ignore.case = TRUE)]          <- NA
  d_res_pretty[d_res_pretty == ""] <- NA

  #Parse ranges
  d_ref <- d[[code_reference]]
  d_res_abn_pretty <- rep(NA, length(d_ref))

  #Identify classes
  w_ste     <- grep("<=", x = d_ref, value = FALSE);       v_ste     <- gsub(".*<=", "",  x = d_ref[w_ste])
  w_st      <- grep("<", x = d_ref, ignore.case = TRUE);   v_st      <- gsub(".*<", "",  x = d_ref[w_st])
  w_lte     <- grep(">=", x = d_ref, value = FALSE);       v_lte     <- gsub(".*>=", "",  x = d_ref[w_lte])
  w_lt      <- grep(">", x = d_ref, ignore.case = TRUE);   v_lt      <- gsub(".*>", "",  x = d_ref[w_lt])
  w_range   <- grep("-", x = d_ref, ignore.case = TRUE);   v_range_u <- gsub(".*-", "",  x = d_ref[w_range])
  w_neg     <- grep("neg", x = d_ref, ignore.case = TRUE); v_range_l <- gsub("-.*", "",  x = d_ref[w_range])

  #Parse values
  d_res_abn_pretty[w_ste] <- suppressWarnings(ifelse(as.numeric(d_res_pretty[w_ste]) <= as.numeric(v_ste), "NORMAL", "ABNORMAL"))
  d_res_abn_pretty[w_st] <- suppressWarnings(ifelse(as.numeric(d_res_pretty[w_st]) < as.numeric(v_st), "NORMAL", "ABNORMAL"))
  d_res_abn_pretty[w_lte] <- suppressWarnings(ifelse(as.numeric(d_res_pretty[w_lte]) >= as.numeric(v_lte), "NORMAL", "ABNORMAL"))
  d_res_abn_pretty[w_lt] <- suppressWarnings(ifelse(as.numeric(d_res_pretty[w_lt]) > as.numeric(v_lt), "NORMAL", "ABNORMAL"))

  d_res_abn_pretty[w_range] <- suppressWarnings(ifelse(as.numeric(d_res_pretty[w_range]) > as.numeric(v_range_l) &
                                                         as.numeric(d_res_pretty[w_range]) < as.numeric(v_range_u), "NORMAL", "ABNORMAL"))
  d_res_abn_pretty[d_res_pretty == "NEG" | d_res_pretty == "BORD"] <- "NORMAL"
  d_res_abn_pretty[d_res_pretty == "POS"] <- "ABNORMAL"
  d_res_abn_pretty[d_ref == d_res] <- "NORMAL"

  d_res_abn_pretty[is.na(d_res_pretty)] <- NA

  #Create abnormal based-on Abnormal_Flag column
  d_flag <- d[[code_flag]]
  d_res_abn_flag_pretty <- rep(NA, length(d_flag))
  d_res_abn_flag_pretty[is.na(d_flag)]  <- "NORMAL"
  d_res_abn_flag_pretty[!is.na(d_flag)] <- "ABNORMAL"


  d[, c("lab_result_pretty", "lab_result_abn_pretty", "lab_result_abn_flag_pretty") := list(d_res_pretty, d_res_abn_pretty, d_res_abn_flag_pretty)]

  return(d)
}
