#' @title Searches columns for given reason for visit defined by ERFV codes.
#' @export
#'
#' @description Analyzes reason for visit data loaded using \emph{load_rfv}.
#' If requested, the data.table is returned with new columns corresponding to boolean values, whether given group of ERFV are present in the given columns.
#' If \emph{collapse} is given, then the information is aggregated based-on the \emph{collapse} column and the earliest of latest time of the given reason for visit is provided.
#'
#' @param d data.table, database containing reason for visit information data loaded using the \emph{load_rfv} function.
#' @param code string vector, an array of column names to search.
#' @param codes_to_find list, a list of arrays corresponding to sets of ERFV codes. The function searches the columns in code and the name of each list element will be created.
#' These columns are indicators whether the given disease is present in the set of ERFV codes or not.
#' @param collapse string, a column name on which to collapse the data.table.
#' Used in case we wish to assess whether multiple ERFV are present within  the same instances of \emph{collapse}. See vignette for details.
#' @param code_time string, column name of the time column. Defaults to \emph{time_rfv_start}. Used in case collapse is present to provide the earliest or latest instance of reason for visit.
#' @param aggr_type string, if multiple reason for visits are present within the same case of \emph{collapse}, which timepoint to return. Supported are: "earliest" or "latest". Defaults to \emph{earliest}.
#' @param nThread integer, number of threads to use for parallelization. If it is set to 1, then no parallel backends are created and the function is executed sequentially.
#'
#' @return data.table, with indicator columns if provided.
#' If \emph{collapse} is present, then only unique ID and the summary columns are returned.
#'
#' @encoding UTF-8
#'
#' @examples \dontrun{
#' #Parse reason for visit columns
#' #and create indicator variables for the following reasons and summarize per patient,
#' #whether there are any encounters where the given reasons were registered
#' reasons <- list(Pain = c("ERFV:160357", "ERFV:140012"), Visit = c("ERFV:501"))
#' data_rfv_disease <-  convert_rfv(d = data_rfv, keep = FALSE,
#' codes_to_find = reasons, nThread = 2, collapse = "ID_MERGE")
#' }

convert_rfv <- function(d, code = "rfv_concept_id", codes_to_find = NULL,
                        collapse = NULL, code_time = "time_rfv_start", aggr_type = "earliest", nThread = parallel::detectCores()-1) {

  .SD=.N=.I=.GRP=.BY=.EACHI=..=..code=.SDcols=i=j=time_to_db=..which_ids_to=..which_ids_from=..collapse=. <- NULL

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

  #Find diagnoses if requested
  if(!is.null(codes_to_find)) {
    message(paste0("Finding reasons within specified columns."))

    #Find diagnoses per row
    result <- foreach::foreach(i = 1:length(codes_to_find), .combine="cbind",
                               .inorder=TRUE, .options.future = list(chunk.size = 1.0),
                               .errorhandling = c("pass"), .verbose=FALSE) %exec%
      {
        if(is.null(collapse)) {
          diag_coll <- d[, any(.SD %in% unlist(codes_to_find[i])), .SDcols = code, by=1:nrow(d)]
          diag_coll$nrow <- NULL
          data.table::setnames(diag_coll, "V1", names(codes_to_find[i]))
          diag_coll
        } else {
          d[, names(codes_to_find[i]) := any(.SD %in% unlist(codes_to_find[i])), .SDcols = code, by=1:nrow(d)]
          ID_dt <- unique(d[, collapse, with = FALSE]) #Get IDs

          if(aggr_type == "earliest") { #Find time
            diag_coll <- d[, .(var_time = min(get(code_time))), by=c(collapse, names(codes_to_find[i]))]
          } else {
            diag_coll <- d[, .(var_time = max(get(code_time))), by=c(collapse, names(codes_to_find[i]))]
          }
          diag_coll <- diag_coll[get(names(codes_to_find[i]))] #Remove negative cases
          diag_coll <- data.table::merge.data.table(ID_dt, diag_coll, by = collapse, all.x = TRUE, all.y = FALSE) #Merge with IDs to get db
          diag_coll[[names(codes_to_find[i])]][is.na(diag_coll[[names(codes_to_find[i])]])] <- FALSE

          data.table::setnames(diag_coll, "var_time", paste0("time_", names(codes_to_find[i])))
          diag_coll
        }
      }
    future::plan(future::sequential)

    if(is.null(collapse)) { #Remove unnecessary info and combine with original data if non-collapse
      result <- cbind(d, result)
    }
    if(!is.null(collapse) & length(codes_to_find)>1) { #Remove unnecessary ID columns if multiple codes_to_find
      result[, seq(4, dim(result)[2], 3)] <- NULL
    }
    return(result)
  } else { #If no diagnoses
    future::plan(future::sequential)
    return(d)
  }
}
