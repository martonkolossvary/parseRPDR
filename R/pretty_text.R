#' @title Removes spaces, special characters and capitals from string vector.
#' @export
#'
#' @description Removes paces, special characters and capitals from string vector and converts unknowns to NA.
#'
#' @param v vector, integer or sting vector with numerical values.
#' @param remove_after boolean whether to remove text after \emph{-}. Defaults to \emph{FALSE}.
#' @param remove_punc boolean, whether to remove punctuation marks. Defaults to \emph{FALSE}.
#' @param remove_white boolean, whether to remove white spaces. Defaults to \emph{FALSE}.
#' @param add_na boolean, whether to change text indicating NA to NA values in R. Defaults to \emph{TRUE}.
#'
#' @return vector, with characters formatted accordingly.
#'
#' @encoding UTF-8

pretty_text <- function(v, remove_after = FALSE, remove_punc = FALSE, remove_white = FALSE, add_na = TRUE) {

  #Set NA values
  if(add_na) {
    na_string <- c("", " ", "NA", "N/A", "@", "Not Found", "UNCLASSIFIED", "UNSPEC", "Other", "***", "No specimen was received for this test.",
                   "Unavailable", "UNAVAILABLE", "Not Asked", "Declined", "Deferred", "Not Determined-ND", "Not Determined-NDU",
                   "Unknown-UNKNOWN", "Unknown-@", "UNKNOWN", "Unknown-U", "Unknown-UNK", "Unknown",
                   "Declined", "Other", "Other-OTHER", "Not reported/refused", "Refused to report",
                   "Not Recorded-DECLINED", "Not Recorded-UN", "Not Recorded-UNKNOWN", "Not Recorded-NOT GIVEN", "Not Recorded-@",
                   "Not Recorded-NOT REPORTED", "Not recorded-@", "Not Recorded-UNK", "Not recorded", "not recorded",
                   "Undefined Codes-00", "Undefined Codes-0", "Undefined Codes-60", "Undefined Codes-A",  ",", ".",
                   "No tim", "NOT DO", "RANDOM", "UNKNOWNUNKNOWN", "Unknown Missing No Info", "UNKNOWN,UNKNOWN", "Unknown, Missing No Info")
    v[v %in% na_string] <- NA
  }

  if(remove_after) {v <- gsub("-.*", "", v, fixed = FALSE)} #Remove text after "-"
  if(remove_punc)  {v <- gsub("[[:punct:]]", "", v)} #Remove punctuation
  if(remove_white) {v <- gsub("[[:space:]]", "", v)} #Remove white space

  #In case of race correct
  v[v == "BLACKORAFRICANAMERICAN"] <- "Black"; v[v == "BLACK OR AFRICAN AMERICAN "] <- "Black"
  v[v == "BLACK OR AFRICAN AMERICAN"] <- "Black"

  return(v)
}
