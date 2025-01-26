#' @title Searches procedures columns for given procedures.
#' @export
#'
#' @description Analyzes procedure data loaded using \emph{load_prc}. Searches procedures columns for a specified set of procedures.
#' By default, the data.table is returned with new columns corresponding to boolean values, whether given group of procedures are present in the given procedure.
#' If \emph{collapse} is given, then the information is aggregated based-on the \emph{collapse} column and the earliest of latest time of the given procedure is provided.
#'
#'
#' @param d data.table, database containing procedure information data loaded using the \emph{load_prc} function.
#' @param code string, column name of the procedure code column. Defaults to \emph{prc_code}.
#' @param code_type string, column name of the code_type column. Defaults to \emph{prc_code_type}.
#' @param codes_to_find list, a list of string arrays corresponding to sets of code types and codes separated by \emph{:}, i.e.: "CPT:00104".
#' The function searches for the given procedure code type and code pair and adds new boolean columns with the name of each list element.
#' These columns are indicators whether any of the procedure code type and code pair occurs in the set of codes.
#' @param collapse string, a column name on which to collapse the data.table.
#' Used in case we wish to assess multiple procedure codes are present within all the same instances of \emph{collapse}. See vignette for details.
#' @param code_time string, column name of the time column. Defaults to \emph{time_prc}. Used in case collapse is present to provide the earliest or latest instance of the given procedure.
#' @param aggr_type string, if multiple procedures are present within the same case of \emph{collapse}, which timepoint to return. Supported are: "earliest" or "latest". Defaults to \emph{earliest}.
#' @param nThread integer, number of threads to use for parallelization. If it is set to 1, then no parallel backends are created and the function is executed sequentially.
#'
#' @return data.table, with indicator columns whether the any of the given procedures are reported.
#' If \emph{collapse} is present, then only unique ID and the summary columns are returned.
#'
#' @encoding UTF-8
#'
#' @examples \dontrun{
#' #Search for Anesthesia CPT codes
#' procedures <- list(Anesthesia = c("CTP:00410", "CPT:00104"))
#' data_prc_parse <- convert_prc(d = data_prc, codes_to_find = procedures, nThread = 2)
#'
#' #Search for Anesthesia CPT codes
#' procedures <- list(Anesthesia = c("CTP:00410", "CPT:00104"))
#' data_prc_procedures <- convert_prc(d = data_prc, codes_to_find = procedures,
#' nThread = 2, collapse = "ID_MERGE", aggr_type = "earliest")
#' }

convert_prc <- function(d, code = "prc_code", code_type = "prc_code_type",  codes_to_find = NULL,
                        collapse = NULL, code_time = "time_prc", aggr_type = "earliest", nThread = parallel::detectCores()-1) {

  .SD=.N=.I=.GRP=.BY=.EACHI=..=..cols=.SDcols=i=j=time_to_db=..which_ids_to=..which_ids_from=combined=..collapse=. <- NULL
  options(future.globals.maxSize = +Inf)

  #Initialize multicore
  if(nThread == 1 | length(codes_to_find) == 1) {
    `%exec%` <- foreach::`%do%`
    future::plan(future::sequential)
  } else {
    if(length(codes_to_find) > 0 & length(codes_to_find) < nThread) {nThread <- length(codes_to_find)}

    if(parallelly::supportsMulticore()) {
      future::plan(future::multicore, workers = nThread)
    } else {
      future::plan(future::multisession, workers = nThread)
    }
    `%exec%` <- doFuture::`%dofuture%`
  }

  #Create combined code colmn
  cols <- c(code_type, code, code_time, collapse)
  comb <- d[, cols, with = FALSE]
  comb[ , combined := do.call(paste, c(.SD, sep = ":")), .SDcols = c(code_type, code)]

  #Find diagnoses if requested
  message(paste0("Finding procedures within specified columns."))

  #Find diagnoses per row
  result <- foreach::foreach(i = 1:length(codes_to_find), .combine="cbind",
                             .inorder=TRUE, .options.future = list(chunk.size = 1.0),
                             .errorhandling = c("pass"), .verbose=FALSE) %exec%
    {
      if(is.null(collapse)) {
        diag_coll <- comb[, any(.SD %in% unlist(codes_to_find[i])), .SDcols = "combined", by=1:nrow(comb)]
        diag_coll$nrow <- NULL
        data.table::setnames(diag_coll, "V1", names(codes_to_find[i]))
        diag_coll
      } else {
        comb[, names(codes_to_find[i]) := any(.SD %in% unlist(codes_to_find[i])), .SDcols = "combined", by=1:nrow(comb)]
        ID_dt <- unique(comb[, collapse, with = FALSE]) #Get IDs

        if(aggr_type == "earliest") { #Find time
          diag_coll <- comb[, .(var_time = min(get(code_time))), by=c(collapse, names(codes_to_find[i]))]
        } else {
          diag_coll <- comb[, .(var_time = max(get(code_time))), by=c(collapse, names(codes_to_find[i]))]
        }
        diag_coll <- diag_coll[get(names(codes_to_find[i]))] #Remove negative cases
        diag_coll <- data.table::merge.data.table(ID_dt, diag_coll, by = collapse, all.x = TRUE, all.y = FALSE) #Merge with IDs to get db
        diag_coll[[names(codes_to_find[i])]][is.na(diag_coll[[names(codes_to_find[i])]])] <- FALSE

        data.table::setnames(diag_coll, "var_time", paste0("time_", names(codes_to_find[i])))
        diag_coll
      }
    }
  on.exit(options(future.globals.maxSize = 1.0 * 1e9))
  future::plan(future::sequential)

  if(is.null(collapse)) { #Remove unnecessary info and combine with original data if non-collapse
    result <- cbind(d, result)
  }
  if(!is.null(collapse) & length(codes_to_find)>1) { #Remove unnecessary ID columns if multiple codes_to_find
    result[, seq(4, dim(result)[2], 3)] <- NULL
  }

  return(result)
}
