#' @title Converts numerical codes to universal format specified by length.
#' @export
#'
#' @description Creates numerical strings with given lengths by removing additional characters from the back and adding leading zeros if necessary.
#'
#' @param v vector, integer or sting vector with numerical values.
#' @param length_final numeric, the length of the final string. Defaults to \emph{5} for zip code conversions.
#' @param remove_from_back numeric, the number of digits to remove from the back of the string. If \emph{NULL}, then removes characters from back more than specified in \emph{length_final}.
#' Defaults to \emph{4} for zip code conversions by removing the add-on codes.
#'
#' @return vector, with characters formatted accordingly.
#'
#' @encoding UTF-8

pretty_numbers <- function(v, length_final = 5, remove_from_back = 4) {

  v <- as.character(v)
  len <- nchar(v)

  #Remove add-on codes by removing last 4 digits if zip is larger than 5
  to_remove <- len > length_final & !is.na(v)
  if(is.null(remove_from_back)) {
    remove_from_back <- len - length_final
    v[to_remove] <- substr(v[to_remove], 1, len[to_remove]-remove_from_back[to_remove])
  } else {
    v[to_remove] <- substr(v[to_remove], 1, len[to_remove]-remove_from_back)
  }

  #Add leading zeros to have a five digits
  len <- nchar(v)
  to_length <- len < length_final & !is.na(v)
  v[to_length] <- paste0(strrep("0", length_final-len[to_length]), v[to_length])

  #Clean up identical values
  is_same <- as.numeric(sapply(v, function(x) length(unique(strsplit(x, "")[[1]]))))
  v[is_same == 1] <- NA

  return(v)
}
