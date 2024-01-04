#' @title Loads all RPDR text outputs into R.
#' @export
#'
#' @description Loads all RPDR text outputs into R and returns a list of data tables processed. If multiple text files of the same type are available (if the query is larger than 25000 patients),
#' then add a \emph{"_"} and a number to merge the same data sources into a single output in the order of the provided number.
#'
#' @param folder string, full folder path to RPDR text files.
#' @param which_data string vector, an array of abbreviation corresponding to the datasources wished to load.
#' @param old_dem boolean, should old \emph{load_dem} function be used for loading demographic data. Defaults to \emph{TRUE}, should be set to \emph{FALSE} for Dem.txt datasets prior to 2022.
#' @param merge_id string, column name to use to create \emph{ID_MERGE} column used to merge different datasets. Defaults to \emph{EMPI},
#' as it is the preferred MRN in the RPDR system. In case of mrn dataset, leave at EMPI, as it is automatically converted to: "Enterprise_Master_Patient_Index".
#' @param sep string, divider between hospital ID and MRN. Defaults to \emph{:}.
#' @param id_length string, indicating whether to modify MRN length based-on required values \emph{id_length = standard}, or to keep lengths as is \emph{id_length = asis}.
#' If \emph{id_length = standard} then in case of \emph{MGH, BWH, MCL, EMPI and PMRN} the length of the MRNs are corrected accordingly by adding zeros, or removing numeral from the beginning.
#' In other cases the lengths are unchanged. Defaults to \emph{standard}.
#' @param perc numeric, a number between 0-1 indicating which parsed ID columns to keep. Data present in \emph{perc x 100\%} of patients are kept.
#' @param na boolean, whether to remove columns with only NA values. Defaults to \emph{TRUE}.
#' @param identical boolean, whether to remove columns with identical values. Defaults to \emph{TRUE}.
#' @param nThread integer, number of threads to use for parallelization.
#' @param many_sources boolean, if \emph{TRUE}, then parallelization is done on the level of the datasources. If \emph{FALSE}, then parallelization is done within the datasources.
#' If there are many datasources, then it is advised to set this TRUE, as then each different datasource will be processed in parallel.
#' However, if there are only a few datasources selected to load, but many files per datasource (result of large queries), then it may be faster to parallelize within each datasource and therefore should be set to \emph{FALSE}.
#' If there are only a few sources each with one file then set to TRUE.
#' @param load_report boolean, should the report text be returned for notes.
#' @param format_orig boolean, should report be returned in its original formatting or should white spaces used for formatting be removed. Defaults to \emph{FALSE}.
#'
#' @return list of parsed data tables containing the information.
#'
#' @encoding UTF-8
#' @examples \dontrun{
#' #Load all Con, Dem and Mrn datasets processing all files within given datasource in parallel
#' load_all_data(folder = folder_rpdr, which_data = c("con", "dem", "mrn"),
#' nThread = 2, many_sources = FALSE)
#'
#' #Load all supported file types parallelizing on the level of datasources
#' load_all_data(folder = folder_rpdr, nThread = 2, many_sources = TRUE,
#' format_orig = TRUE)
#' }

load_all_data <- function(folder, which_data = c("mrn", "con", "dem", "all", "bib", "dia", "enc", "lab", "lno", "mcm", "med", "mic", "phy", "prc", "prv", "ptd", "rdt", "rfv", "trn",
                                                 "car", "dis", "end", "hnp", "opn", "pat", "prg", "pul", "rad", "vis"), old_dem = FALSE,
                          merge_id = "EMPI", sep = ":", id_length = "standard", perc = 0.6, na = TRUE, identical = TRUE, nThread = parallel::detectCores()-1, many_sources = TRUE, load_report = TRUE, format_orig = FALSE) {

  .SD=.N=.I=.GRP=.BY=.EACHI=..=..cols=.SDcols=i=j=time_to_db=..which_ids_to=..which_ids_from <- NULL

  load_functions <- paste0("load_", which_data)
  load_functions[grep(paste0(c("car", "dis", "end", "hnp", "opn", "pat", "prg", "pul", "rad", "vis"), collapse = "|"), load_functions)] <- "load_notes" #Change to load_notes if needed
  l_df <- sapply(which_data, function(x) NULL)

  files_short <- list.files(folder, full.names = FALSE, pattern = ".txt")
  files_long  <- list.files(folder, full.names = TRUE, pattern = ".txt")

  message(paste0("Loading ", paste0(which_data, collapse = ", "), " datasorces."))

  if(many_sources) {
    if(length(which_data) < nThread) {nThread <- length(which_data)}
    #Initialize clusters
    if(nThread == 1) {
      `%exec_inner%` <- foreach::`%do%`; `%exec_outer%` <- foreach::`%do%`
      future::plan(future::sequential)
    } else {
      if(parallelly::supportsMulticore()) {
        future::plan(future::multicore, workers = nThread)
      } else {
        future::plan(future::multisession, workers = nThread)
      }
      `%exec_inner%` <- foreach::`%do%`; `%exec_outer%` <- doFuture::`%dofuture%`
    }

    result <- foreach::foreach(i = 1:length(l_df), .inorder=TRUE, .options.future = list(globals = TRUE, chunk.size = 1.0,
                                                                                         packages = c("parseRPDR", "foreach", "doFuture")),
                               .errorhandling = c("pass"), .verbose=FALSE) %exec_outer%
      {
        type <- names(l_df)[i]

        files_type <- grep(paste0(type, ".*txt"), x = tolower(files_short), value = FALSE) #select all files of given type
        #If lab data then consider alternative file names
        if(type == "lab" & length(files_type) == 0) {
          alt_lab <- c("clb")
          files_type <- NULL
          for(j in alt_lab) {files_type <- c(files_type, grep(paste0(alt_lab, ".*txt"), x = tolower(files_short), value = FALSE))}
        }
        #If enc data then consider alternative file names
        if(type == "enc" & length(files_type) == 0) {
          alt_lab <- c("exc")
          files_type <- NULL
          for(j in alt_lab) {files_type <- c(files_type, grep(paste0(alt_lab, ".*txt"), x = tolower(files_short), value = FALSE))}
        }
        #If dia data then consider alternative file names
        if(type == "dia" & length(files_type) == 0) {
          alt_lab <- c("dea")
          files_type <- NULL
          for(j in alt_lab) {files_type <- c(files_type, grep(paste0(alt_lab, ".*txt"), x = tolower(files_short), value = FALSE))}
        }
        #If med data then consider alternative file names
        if(type == "med" & length(files_type) == 0) {
          alt_lab <- c("mee")
          files_type <- NULL
          for(j in alt_lab) {files_type <- c(files_type, grep(paste0(alt_lab, ".*txt"), x = tolower(files_short), value = FALSE))}
        }
        #If prc data then consider alternative file names
        if(type == "prc" & length(files_type) == 0) {
          alt_lab <- c("pec")
          files_type <- NULL
          for(j in alt_lab) {files_type <- c(files_type, grep(paste0(alt_lab, ".*txt"), x = tolower(files_short), value = FALSE))}
        }

        numb <- suppressWarnings(as.numeric(gsub(".*_(.+).txt", "\\1", files_short[files_type]))) #get order of files
        files_long_type <-files_long[files_type[order(numb)]] #select and order full file paths


        result <- foreach::foreach(j = 1:length(files_long_type), .inorder=TRUE, .options.future = list(globals = TRUE, chunk.size = 1.0,
                                                                                                        packages = c("parseRPDR", "foreach", "doFuture")),
                                   .errorhandling = c("pass"), .verbose=FALSE) %exec_inner%
          {
            if(length(files_long_type) != 0) {
              if(type %in% c("car", "dis", "end", "hnp", "opn", "pat", "prg", "pul", "rad", "vis")) {
                func <- "load_notes"
                l_i <- eval(str2expression(paste0(func, "(\"", files_long_type[j], "\", ",
                                                  "type = \"", type, "\", ",
                                                  "merge_id = \"", merge_id, "\", ",
                                                  "sep = \"", sep, "\", ",
                                                  "id_length = \"", id_length, "\", ",
                                                  "perc = ", perc, ", ",
                                                  "nThread = ", 1, ", ",
                                                  "load_report = \"", load_report, "\", ",
                                                  "format_orig = \"", format_orig, "\", ",
                                                  "na = FALSE, identical = FALSE)")))
              } else {
                func <- grep(type, x = tolower(load_functions), value = TRUE, fixed = TRUE)
                if(old_dem & type == "dem") {func <- "load_dem_old"}
                l_i <- eval(str2expression(paste0(func, "(\"", files_long_type[j], "\", ",
                                                  "merge_id = \"", merge_id, "\", ",
                                                  "sep = \"", sep, "\", ",
                                                  "id_length = \"", id_length, "\", ",
                                                  "perc = ", perc, ", ",
                                                  "nThread = ", 1, ", ",
                                                  "na = FALSE, identical = FALSE)")))
              }
              l_i
            }
          }
        result <- data.table::rbindlist(result, use.names = TRUE, fill = TRUE) #merge data sources
        result <- remove_column(dt = result, na = na, identical = identical) #remove columns if necessary
        result
      }
    future::plan(future::sequential)

  } else { #Parallelize inner loop

    `%exec_outer%` <- foreach::`%do%`

    result <- foreach::foreach(i = 1:length(l_df), .inorder=TRUE, .options.future = list(globals = TRUE, chunk.size = 1.0,
                                                                                         packages = c("parseRPDR", "foreach", "doFuture")),
                               .errorhandling = c("pass"), .verbose=FALSE) %exec_outer%
      {
        type <- names(l_df)[i]

        files_type <- grep(paste0(type, ".*txt"), x = tolower(files_short), value = FALSE) #select all files of given type
        #If lab data then consider alternative file names
        if(type == "lab" & length(files_type) == 0) {
          alt_lab <- c("clb")
          files_type <- NULL
          for(j in alt_lab) {files_type <- c(files_type, grep(paste0(alt_lab, ".*txt"), x = tolower(files_short), value = FALSE))}
        }
        #If enc data then consider alternative file names
        if(type == "enc" & length(files_type) == 0) {
          alt_lab <- c("exc")
          files_type <- NULL
          for(j in alt_lab) {files_type <- c(files_type, grep(paste0(alt_lab, ".*txt"), x = tolower(files_short), value = FALSE))}
        }
        #If dia data then consider alternative file names
        if(type == "dia" & length(files_type) == 0) {
          alt_lab <- c("dea")
          files_type <- NULL
          for(j in alt_lab) {files_type <- c(files_type, grep(paste0(alt_lab, ".*txt"), x = tolower(files_short), value = FALSE))}
        }
        #If med data then consider alternative file names
        if(type == "med" & length(files_type) == 0) {
          alt_lab <- c("mee")
          files_type <- NULL
          for(j in alt_lab) {files_type <- c(files_type, grep(paste0(alt_lab, ".*txt"), x = tolower(files_short), value = FALSE))}
        }
        if(type == "prc" & length(files_type) == 0) {
          alt_lab <- c("pec")
          files_type <- NULL
          for(j in alt_lab) {files_type <- c(files_type, grep(paste0(alt_lab, ".*txt"), x = tolower(files_short), value = FALSE))}
        }

        numb <- suppressWarnings(as.numeric(gsub(".*_(.+).txt", "\\1", files_short[files_type]))) #get order of files
        files_long_type <-files_long[files_type[order(numb)]] #select and order full file paths

        #Initiate clusters
        if(length(files_long_type) < nThread) {nThread <- length(files_long_type)}
        if(nThread == 1) {
          `%exec_inner%` <- foreach::`%do%`
          future::plan(future::sequential)
        } else {
          if(parallelly::supportsMulticore()) {
            future::plan(future::multicore, workers = nThread)
          } else {
            future::plan(future::multisession, workers = nThread)
          }
          `%exec_inner%` <- doFuture::`%dofuture%`
        }

        result_inner <- foreach::foreach(j = 1:length(files_long_type), .inorder=TRUE, .options.future = list(globals = TRUE, chunk.size = 1.0,
                                                                                                              packages = c("parseRPDR", "foreach", "doFuture")),
                                         errorhandling = c("pass"), .verbose=FALSE) %exec_inner%
          {
            if(length(files_long_type) != 0) {
              if(type %in% c("car", "dis", "end", "hnp", "opn", "pat", "prg", "pul", "rad", "vis")) {
                func <- "load_notes"
                l_i <- eval(str2expression(paste0(func, "(\"", files_long_type[j], "\", ",
                                                  "type = \"", type, "\", ",
                                                  "merge_id = \"", merge_id, "\", ",
                                                  "sep = \"", sep, "\", ",
                                                  "id_length = \"", id_length, "\", ",
                                                  "perc = ", perc, ", ",
                                                  "nThread = ", 1, ", ",
                                                  "load_report = \"", load_report, "\", ",
                                                  "format_orig = \"", format_orig, "\", ",
                                                  "na = FALSE, identical = FALSE)")))
              } else {
                func <- grep(type, x = tolower(load_functions), value = TRUE, fixed = TRUE)
                if(old_dem & type == "dem") {func <- "load_dem_old"}
                l_i <- eval(str2expression(paste0(func, "(\"", files_long_type[j], "\", ",
                                                  "merge_id = \"", merge_id, "\", ",
                                                  "sep = \"", sep, "\", ",
                                                  "id_length = \"", id_length, "\", ",
                                                  "perc = ", perc, ", ",
                                                  "nThread = ", 1, ", ",
                                                  "na = FALSE, identical = FALSE)")))
              }
              l_i
            }
          }
        future::plan(future::sequential)
        result_inner <- data.table::rbindlist(result_inner, use.names = TRUE, fill = TRUE) #merge data sources
        result_inner <- remove_column(dt = result_inner, na = na, identical = identical) #remove columns if necessary
        result_inner
      }
  }

  #Add names and remove unnecessary columns
  names(result) <- which_data
  result <- result[lengths(result) != 0]

  return(result)
}
