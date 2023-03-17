#' @title Parse IDs from a string of delimited list of values.
#' @keywords internal
#'
#' @description Creates columns corresponding to MRNs in the string of delimited list of values. If the string and the numeric part of the MRN
#' are present in the same column, then supply the column to \emph{str}. If the string portion and the numeric portion is in different columns, then
#' supply the string part to \emph{str} and the numeric part to \emph{num}.
#' @param str vector, delimited list of MRN string values.
#' @param num vector, delimited list of MRN numeric values.
#' @param sep string, divider between hospital ID and MRN. Defaults to \emph{:}.
#' @param id_length string, indicating whether to modify MRN length based-on required values \emph{id_length = standard}, or to keep lengths as is \emph{id_length = asis}.
#' If \emph{id_length = standard} then in case of \emph{MGH, BWH, MCL, EMPI and PMRN} the length of the MRNs are corrected accordingly by adding zeros, or removing numeral from the beginning.
#' In other cases the lengths are unchanged. Defaults to \emph{standard}.
#' @param perc numeric, a number between 0-1 indicating which parsed ID columns to keep. Columns present in \emph{perc x 100\%} of patients have are kept.
#' @param nThread integer, number of threads to use by \emph{dopar} for parallelization. If it is set to 1, then no parallel backends are created and the function is executed sequentially.

#' @return data table, with columns corresponding to MRNs in the string of delimited list of values.
#'
#' @encoding UTF-8
#' @importFrom data.table :=

parse_ids <- function(str, num = NULL, sep = ":", id_length = "standard", perc = 0.6, nThread = parallel::detectCores()-1) {

  .SD=.N=.I=.GRP=.BY=.EACHI=..=..cols=.SDcols=i=j=time_to_db=..which_ids_to=..which_ids_from <- NULL

  #Initialize multicore
  if(nThread == 1) {
    `%exec%` <- foreach::`%do%`
  } else {
    cl <- parallel::makeCluster(nThread, methods = FALSE, useXDR = FALSE)
    doParallel::registerDoParallel(cl)
    `%exec%` <- foreach::`%dopar%`
  }

  #Parse Patient_ID_List to get IDs above ID_perc_available prevalence
  HospIDs_str <- stringr::str_match_all(str, "[[:alpha:]]+") #get hospital IDs
  if(is.null(num)) {
    HospIDs_numb <- stringr::str_match_all(str, "[[:digit:]]+") #get hospital IDs
  } else {
    HospIDs_numb <- stringr::str_match_all(num, "[[:digit:]]+") #get hospital IDs
  }

  HospIDs_str_incl <- table(unlist(HospIDs_str))/length(str) > perc
  HospIDs_str_incl <- names(HospIDs_str_incl)[HospIDs_str_incl] #get hospital IDs over given prevalence threshold

  HospIDs_str_boo <- lapply(HospIDs_str, stringr::str_detect, paste(HospIDs_str_incl, collapse = '|'),) #create boo
  HospIDs_str_boo <- lapply(HospIDs_str_boo, t); HospIDs_str_boo <- lapply(HospIDs_str_boo, t)
  HospIDs_str_inc_boo <- Map(`[`, HospIDs_str, HospIDs_str_boo) #filter
  HospIDs_num_inc_boo <- Map(`[`, HospIDs_numb, HospIDs_str_boo)

  #create df from list values
  message("Parsing all possible IDs.")

  result <- foreach::foreach(i = 1:length(HospIDs_str_inc_boo), .combine="rbind",
                             .inorder=TRUE,
                             .errorhandling = c("pass"), .verbose=FALSE) %exec%
    {
      df_ids <- data.table::data.table(matrix(as.character(NA), nrow = 1, ncol = length(HospIDs_str_incl)))
      colnames(df_ids) <- HospIDs_str_incl
      for(j in 1:length(HospIDs_str_inc_boo[[i]])) {
        if(length(HospIDs_num_inc_boo[[i]][j]) != 0 & length(HospIDs_str_inc_boo[[i]][j] != 0)) {
          df_ids[1, HospIDs_str_inc_boo[[i]][j] := pretty_mrn(v = HospIDs_num_inc_boo[[i]][j], prefix = HospIDs_str_inc_boo[[i]][j], sep = sep, id_length = id_length, nThread = 1)]
        }
      }
      df_ids
    }

  if(exists("cl") & nThread>1) {parallel::stopCluster(cl)}
  return(result)
}
