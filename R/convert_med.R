#' @title Adds boolean columns corresponding to a group of medications whether it is present in the given row.
#' @export
#'
#' @description Analyzes medication data loaded using \emph{load_med}.
#' By default, the data.table is returned with new columns corresponding to boolean values, whether given group of medications are present.
#' If \emph{collapse} is given, then the information is aggregated based-on the \emph{collapse} column and the earliest of latest time of the given medication is provided.
#'
#' @param d data.table, database containing medication data loaded using the \emph{load_med} function.
#' @param code string, column name of the medication column. Defaults to \emph{med}.
#' @param codes_to_find list, a list of arrays corresponding to sets of medication names. New boolean columns with the name of each list element will be created.
#' These columns are indicators whether the given medication is present in the set of medication names or not.
#' @param collapse string, a column name on which to collapse the data.table.
#' Used in case we wish to assess whether given medications are present within all the same instances of \emph{collapse}. See vignette for details.
#' @param code_time string, column name of the time column. Defaults to \emph{time_med}. Used in case collapse is present to provide the earliest or latest instance of diagnosing the given disease.
#' @param aggr_type string, if multiple occurences of the medications are present within the same case of \emph{collapse}, which timepoint to return. Supported are: "earliest" or "latest". Defaults to \emph{earliest}.
#' @param nThread integer, number of threads to use for parallelization. If it is set to 1, then no parallel backends are created and the function is executed sequentially.
#'
#' @return data.table, with indicator columns whether given group of codes_to_find is present or not.
#' If \emph{collapse} is present, then only unique ID and the summary columns are returned.
#'
#' @encoding UTF-8
#'
#' @examples \dontrun{
#' #Define medication group and add an indicator column whether
#' #the given medication group was administered
#' meds <- list(statin = c("Simvastatin", "Atorvastatin"),
#'              NSAID  = c("Acetaminophen", "Paracetamol"))
#'
#' data_med_indic <- convert_med(d = data_med, codes_to_find = meds, nThread = 1)
#'
#' #Summarize per patient if they ever had the given medication groups registered
#' data_med_indic_any <- convert_med(d = data_med,
#' codes_to_find = meds, collapse = "ID_MERGE", nThread = 2)
#' }

convert_med <- function(d, code = "med", codes_to_find = NULL, collapse = NULL,
                        code_time = "time_med", aggr_type = "earliest", nThread = parallel::detectCores()-1) {

  .SD=.N=.I=.GRP=.BY=.EACHI=..=..cols=.SDcols=i=j=time_to_db=..which_ids_to=..which_ids_from=..collapse=. <- NULL

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

  #Create columns
  cols <- c(code, code_time, collapse)
  comb <- d[, cols, with = FALSE]

  #Find codes_to_find
  message(paste0("Finding medications."))

  result <- foreach::foreach(i = 1:length(codes_to_find), .combine="cbind",
                             .inorder=TRUE, .options.future = list(chunk.size = 1.0, globals = structure(TRUE, add = "comb")),
                             .errorhandling = c("pass"), .verbose=FALSE) %exec%
    {
      reg_exp <- paste(unlist(codes_to_find[i]), collapse="|")
      matches <- grep(reg_exp, comb[[code]], ignore.case = TRUE)
      med_boo <- rep(FALSE, dim(comb)[1])
      med_boo[matches] <- TRUE
      med_boo <- data.table::as.data.table(med_boo)
      data.table::setnames(med_boo, "med_boo", names(codes_to_find[i]))

      if(is.null(collapse)) {
        med_boo
      } else {
        comb <- cbind(comb, med_boo)
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

  future::plan(future::sequential)

  if(is.null(collapse)) { #Remove unnecessary info and combine with original data if non-collapse
    result <- cbind(d, result)
  }
  if(!is.null(collapse) & length(codes_to_find)>1) { #Remove unnecessary ID columns if multiple codes_to_find
    result[, seq(4, dim(result)[2], 3)] <- NULL
  }
  return(result)
}
