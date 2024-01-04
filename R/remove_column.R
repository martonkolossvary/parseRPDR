#' @title Delete columns with all NA or all identical data.
#' @export
#'
#' @description Delete columns where all data elements are NA or the same.
#'
#' @param dt data.table, to manipulate.
#' @param na boolean, to delete columns where all data elements are NA.
#' @param identical boolean, to delete columns where all data elements are the same.
#'
#' @return data table, with data.
#'
#' @encoding UTF-8
#' @importFrom data.table :=

remove_column <- function(dt, na = TRUE, identical = TRUE) {
  if(na) {
    na_cols <- sapply(dt, function(x) all(is.na(x)))
    if(any(na_cols)) {dt[, (colnames(dt)[na_cols]):=NULL]}
  }
  if(identical) {
    identical_cols <- sapply(dt, function(x) length(unique(x)) == 1)
    if(any(identical_cols)) {dt[, (colnames(dt)[identical_cols]):=NULL]}
  }
  return(dt)
}
