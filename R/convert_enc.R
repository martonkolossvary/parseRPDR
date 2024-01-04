#' @title Searches columns for given diseases defined by ICD codes.
#' @export
#'
#' @description Analyzes encounter data loaded using \emph{load_enc}. Converts columns with ICD codes and text to simple ICD codes.
#' If requested, the data.table is returned with new columns corresponding to boolean values, whether given group of diagnoses are present in the given columns.
#' If \emph{collapse} is given, then the information is aggregated based-on the \emph{collapse} column and the earliest of latest time of the given diagnosis is provided.
#'
#' @param d data.table, database containing encounter information data loaded using the \emph{load_enc} function.
#' @param code string vector, an array of column names to convert to simple ICD codes. The new column names will be the old one with \emph{ICD_} added to the beginning of it.
#' @param keep boolean, whether to keep original columns that were converted. Defaults to \emph{FALSE}.
#' @param codes_to_find list, a list of arrays corresponding to sets of ICD codes. The function searches the columns in code and new boolean columns with the name of each list element will be created.
#' These columns are indicators whether the given disease is present in the set of ICD codes or not.
#' @param collapse string, a column name on which to collapse the data.table.
#' Used in case we wish to assess whether given diagnoses are present within all the same instances of \emph{collapse}. See vignette for details.
#' @param code_time string, column name of the time column. Defaults to \emph{time_enc_admit}. Used in case collapse is present to provide the earliest or latest instance of diagnosing the given disease.
#' @param aggr_type string, if multiple diagnoses are present within the same case of \emph{collapse}, which timepoint to return. Supported are: "earliest" or "latest". Defaults to \emph{earliest}.
#' @param nThread integer, number of threads to use for parallelization. If it is set to 1, then no parallel backends are created and the function is executed sequentially.
#'
#' @return data.table, with formatted ICD code columns and possibly indicator columns if provided.
#' If \emph{collapse} is present, then only unique ID and the summary columns are returned.
#'
#' @encoding UTF-8
#'
#' @examples \dontrun{
#' #Parse encounter ICD columns and keep original ones as well
#' data_enc_parse <- convert_enc(d = data_enc, keep = TRUE, nThread = 2)
#'
#' #Parse encounter ICD columns and discard original ones,
#' #and create indicator variable for the following diseases
#' diseases <- list(HT = c("I10"), Stroke = c("434.91", "I63.50"))
#' data_enc_disease <-  convert_enc(d = data_enc, keep = FALSE,
#' codes_to_find = diseases, nThread = 2)
#'
#' #Parse encounter ICD columns and discard original ones
#' #and create indicator variables for the following diseases and summarize per patient,
#' #whether there are any encounters where the given diseases were registered
#' diseases <- list(HT = c("I10"), Stroke = c("434.91", "I63.50"))
#' data_enc_disease <-  convert_enc(d = data_enc, keep = FALSE,
#' codes_to_find = diseases, nThread = 2, collapse = "ID_MERGE")
#' }

convert_enc <- function(d, code = c("enc_diag_admit", "enc_diag_princ", paste0("enc_diag_", 1:10)),
                        keep = FALSE, codes_to_find = NULL,
                        collapse = NULL, code_time = "time_enc_admit", aggr_type = "earliest", nThread = parallel::detectCores()-1) {

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

  #Create ICD codes
  icd <- d[, code, with = FALSE]
  icd <- apply(icd, 2, function(x) gsub(pattern = " -.*", replacement = "", x = x))
  colnames(icd) <- paste0("ICD_", colnames(icd))

  #Delete columns if requested
  if(!keep) {
    d[, (code)] <- NULL
  }

  #Merge data
  d <- cbind(d, icd)

  #Find diagnoses if requested
  if(!is.null(codes_to_find)) {
    message(paste0("Finding diagnoses within specified columns."))

    #Find diagnoses per row
    result <- foreach::foreach(i = 1:length(codes_to_find), .combine="cbind",
                               .inorder=TRUE, .options.future = list(chunk.size = 1.0),
                               .errorhandling = c("pass"), .verbose=FALSE) %exec%
      {
        if(is.null(collapse)) {
          diag_coll <- d[, any(.SD %in% unlist(codes_to_find[i])), .SDcols = colnames(icd), by=1:nrow(d)]
          diag_coll$nrow <- NULL
          data.table::setnames(diag_coll, "V1", names(codes_to_find[i]))
          diag_coll
        } else {
          d[, names(codes_to_find[i]) := any(.SD %in% unlist(codes_to_find[i])), .SDcols = colnames(icd), by=1:nrow(d)]
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
