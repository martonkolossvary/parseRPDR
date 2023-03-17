#' @title Find exam data within a given timeframe using parallel CPU computing with shared RAM management.
#' @keywords internal
#'
#' @description Finds all, earliest or closest examination to a given timepoints using parallel computing
#'
#' @param d_from data table, the database which is searched to find examinations within the timeframe.
#' @param d_to data table, the database to which we wish to find examinations within the timeframe.
#' @param d_from_ID string, column name of the patient ID column in d_from. Defaults to \emph{ID_MERGE}.
#' @param d_to_ID string, column name of the patient ID column in d_to. Defaults to \emph{ID_MERGE}.
#' @param d_from_time string, column name of the time variable column in d_from. Defaults to \emph{time_rad_exam}.
#' @param d_to_time string, column name of the time variable column in d_to. Defaults to \emph{time_enc_admit}.
#' @param time_diff_name string, column name of the new column created which holds the time difference between the exam and the time provided by d_to. Defaults to \emph{timediff_exam_to_db}.
#' @param before boolean, should times before the given time be considered. Defaults to \emph{TRUE}.
#' @param after boolean, should times after the given time be considered. Defaults to \emph{TRUE}.
#' @param time integer, the timeframe considered between the exam and the d_to timepoints. Defaults to \emph{1}.
#' @param time_unit string, the unit of time used. It is passed on to the \emph{units} argument of \emph{difftime}. "secs", "mins", "hours",
#' "days" and "weeks" are supported.
#' @param multiple string, which exams to give back. \emph{closest} gives back the exam closest to the time provided by d_to.
#' \emph{all} gives back all occurrences within the timeframe. \emph{earliest} the earliest exam within the timeframe.
#' In case of ties for \emph{closest} or \emph{earliest}, all are returned. Defaults to \emph{closest}.
#' @param add_column string, a column name in d_to to add to the output. Defaults to \emph{NULL}.
#' @param keep_data boolean, whether to include empty rows with only the \emph{d_from_ID} column filed out for cases that have data in the \emph{d_from}, but not within the time range. Defaults to \emph{FALSE}.
#' @param nThread integer, number of threads to use by \emph{dopar} for parallelization. If it is set to 1, then no parallel backends are created and the function is executed sequentially.
#'
#' @return data table, with \emph{d_from} filtered to ones only within the timeframe. The columns of \emph{d_from} are returned with the corresponding time column in \emph{data_to}
#' where the rows are instances which comply with the time constraints specified by the function. An additional column specified in \emph{time_diff_name} is also returned,
#' which shows the time difference between the time column in \emph{d_from} and \emph{d_to} for that given case.
#' Also the time column from \emph{d_to} specified by \emph{d_to_time} is returned under the name of \emph{time_to_db}.
#' An additional column specified in \emph{add_column} may be added from \emph{data_to} to the data table.
#'
#' @encoding UTF-8

find_exam_bm <- function(d_from, d_to,
                         d_from_ID = "ID_MERGE", d_to_ID = "ID_MERGE",
                         d_from_time = "time_rad_exam", d_to_time = "time_enc_admit",
                         time_diff_name = "timediff_exam_to_db", before = TRUE, after = TRUE, time = 1, time_unit = "days",
                         multiple = "closest", add_column = NULL, keep_data = FALSE, nThread = parallel::detectCores()-1) {

  .SD=.N=.I=.GRP=.BY=.EACHI=..=..cols=.SDcols=i=j=time_to_db=..which_ids_to=..which_ids_from=..collapse <- NULL

  #Initialize multicore
  cl <- parallel::makeCluster(nThread, methods = FALSE, useXDR = FALSE)
  doParallel::registerDoParallel(cl)
  `%exec%` <- foreach::`%dopar%`

  #Convert to bigmemory compatible format
  d_from <- data.table::copy(d_from)
  d_to   <- data.table::copy(d_to)

  which_ids_from_time <- sapply(sapply(d_from, class), `%in%`, x = "POSIXt") #Get time variables
  which_ids_from_time <- names(which_ids_from_time)[which_ids_from_time]
  which_ids_from  <- colnames(d_from) #Convert columns to factor in d_from
  d_from[, (which_ids_from):= lapply(.SD, as.factor), .SDcols = which_ids_from]
  factor_la_from <- sapply(d_from[, which_ids_from, with = FALSE], levels) #Labels
  factor_le_from <- sapply(factor_la_from, length) #Levels
  factor_la_from_time <-  factor_la_from[which_ids_from_time]#Labels-time
  factor_le_from_time <- factor_le_from[which_ids_from_time] #Levels-time
  factor_la_from_ID <-  factor_la_from[d_from_ID]#Labels-ID
  d_from[, (which_ids_from):= lapply(.SD, as.numeric), .SDcols = which_ids_from]

  which_ids_to_time <- sapply(sapply(d_to, class), `%in%`, x = "POSIXt") #Get time variables
  which_ids_to_time <- names(which_ids_to_time)[which_ids_to_time]
  which_ids_to     <- colnames(d_to) #Convert columns to factor in d_to
  d_to[, (which_ids_to):= lapply(.SD, as.factor), .SDcols = which_ids_to]
  factor_la_to <- sapply(d_to[, which_ids_to, with = FALSE], levels) #Labels
  factor_le_to <- sapply(factor_la_to, length) #Levels
  factor_la_to_time <-  factor_la_to[which_ids_to_time]#Labels-time
  factor_le_to_time <- factor_le_to[which_ids_to_time] #Levels-time
  factor_la_to_ID <-  factor_la_to[d_to_ID]#Labels-ID
  d_to[, (which_ids_to):= lapply(.SD, as.numeric), .SDcols = which_ids_to]

  #Initiate output
  empty <- d_from[FALSE, ]
  empty[, (time_diff_name) := difftime(NULL, NULL, units = time_unit)]
  empty[, time_to_db := as.POSIXct(NULL)]
  empty[, (which_ids_from_time):= lapply(.SD, as.POSIXct), .SDcols = which_ids_from_time]
  if (!is.null(add_column)) {empty[, (add_column):=character()]}
  out <- empty; i = 1

  #Create iterator
  if(dim(d_to)[1]<100) {
    blocks <- list(1:dim(d_to)[1])
  } else {
    groups <- cut(1:dim(d_to)[1], breaks = nThread, labels = 1:nThread)
    ids    <- 1:dim(d_to)[1]
    blocks <- split(ids, groups)
  }


  #Create bigmemory matrices
  d_from <- suppressWarnings(bigmemory::as.big.matrix(x = d_from, type = "double", shared = TRUE, separated = FALSE))
  d_to   <- suppressWarnings(bigmemory::as.big.matrix(x = d_to, type = "double", shared = TRUE, separated = FALSE))
  d_from_desc <- bigmemory::describe(d_from); d_from_desc@description$rowNames <- NULL
  d_to_desc   <- bigmemory::describe(d_to);   d_to_desc@description$rowNames <- NULL

  message(paste0("Finding ", multiple, " data within ", time, " ", time_unit, "."))

  result <- foreach::foreach(j = 1:length(blocks), .combine="rbind",
                             .inorder=TRUE,
                             .errorhandling = c("pass"), .verbose=FALSE) %exec%
    {
      d_from_foreach  <- bigmemory::attach.big.matrix(d_from_desc)
      d_to_foreach    <- bigmemory::attach.big.matrix(d_to_desc)

      get_ids <- blocks[[j]]
      Exams <- NULL

      for(i in get_ids) {

        #Get ID codes
        ID_to_i         <- factor_la_to_ID[[d_to_ID]][d_to_foreach[i, d_to_ID]]
        ID_code_from_i  <- which(factor_la_from_ID[[d_from_ID]] == ID_to_i)

        if(length(ID_code_from_i) > 0){ #If data present

          #Locate i-th case for d_from
          id_from <- bigmemory::mwhich(d_from_foreach, cols = d_from_ID, vals = ID_code_from_i, comps = "eq")
          Exam_i <- d_from_foreach[id_from, ]
          if(is.null(dim(Exam_i))) { #if only one row available
            Exam_i <- data.table::as.data.table(as.list(Exam_i))
          } else {
            Exam_i <- data.table::as.data.table(Exam_i)
          }
          time_i <- lapply(which_ids_from_time, function(x) { #Convert only time back
            if(all(is.na(Exam_i[[x]]))) {
              out <- as.character(Exam_i[[x]])
            } else {
              out <- factor(Exam_i[[x]],
                            labels = as.character(unlist(factor_la_from_time[x])),
                            levels = 1:factor_le_from_time[x])
              out <- as.character(out)
            }
          })
          time_i <- data.table::as.data.table(time_i); colnames(time_i) <- names(factor_la_from_time)
          Exam_i[, (which_ids_from_time):= time_i]
          Exam_i[, (which_ids_from_time):= lapply(.SD, as.POSIXct), .SDcols = which_ids_from_time]

          #Locate ith case for d_to
          d_to_i <- d_to_foreach[i, ]; d_to_i <- data.table::as.data.table(as.list(d_to_i))
          time_i <- lapply(which_ids_to_time, function(x) { #Convert only time back
            if(all(is.na(d_to_i[[x]]))) {
              out <- as.character(d_to_i[[x]])
            } else {
              out <- factor(d_to_i[[x]],
                            labels = as.character(unlist(factor_la_to_time[x])),
                            levels = 1:factor_le_to_time[x])
              out <- as.character(out)
            }
          })
          time_i <- data.table::as.data.table(time_i); colnames(time_i) <- names(factor_la_to_time)
          d_to_i[, (which_ids_to_time):= time_i]
          d_to_i[, (which_ids_to_time):= lapply(.SD, as.POSIXct), .SDcols = which_ids_to_time]

          #Calculate time differences
          dif_i <- difftime(trunc.POSIXt(Exam_i[, get(d_from_time)], units = time_unit),
                            trunc.POSIXt(d_to_i[, get(d_to_time)], units = time_unit), units = time_unit)


          #Filter if before or after index event
          if(!after) {
            dif_i[dif_i > 0] <- NA
          } else if(!before) {
            dif_i[dif_i < 0] <- NA
          }
          dif_i[abs(dif_i) > time] <- NA #Get only within time window

          #Add time differences and the time in d_to which was used for pairing
          Exam_i[[time_diff_name]] <- dif_i
          Exam_i[["time_to_db"]] <- d_to_i[, get(d_to_time)]

          #If to add a column from the database used for reference
          if(!is.null(add_column)) {
            Exam_i[[add_column]] <- d_to_i[, get(add_column)]
          }

          #Subselect which data to keep
          if(multiple == "closest") {
            Exams_i_sel <- Exam_i[which.min(abs(dif_i))]
          } else if(multiple == "all") {
            Exams_i_sel <- Exam_i[!is.na(get(time_diff_name))]
          } else if(multiple == "earliest") {
            Exams_i_sel <- Exam_i[which.min(dif_i)]
          }

          #If IDs of cases without data in the given timeframe should also be included in the output
          if(keep_data & dim(Exams_i_sel)[1] == 0) {
            empty_i <- data.table::copy(empty)
            empty_i <- empty_i[1, ]
            if(!is.null(add_column)) {
              empty_i[[add_column]] <- d_to_i[, get(add_column)]
            }
            empty_i[[d_from_ID]] <- Exam_i[1, get(d_from_ID)]
            Exams_i_sel <- empty_i
          }
          Exams <- rbind(Exams, Exams_i_sel)
        }
      }
      rm(ID_to_i, ID_code_from_i, id_from, Exam_i, time_i, d_to_i, dif_i, Exams_i_sel)
      gc(full = TRUE)

      Exams
    }
  if(exists("cl") & nThread>1) {parallel::stopCluster(cl)}
  rm(d_from, d_to); gc(full = TRUE)

  #Convert back to original codes
  conv_back_ids <- which_ids_from[!(which_ids_from %in% c(which_ids_from_time))]
  result_conv <- lapply(conv_back_ids, function(x) {
    if(all(is.na(result[[x]]))) {
      out <- as.character(result[[x]])
    } else {
      out <- factor(result[[x]],
                    labels = as.character(unlist(factor_la_from[x])),
                    levels = 1:factor_le_from[x])
      factor_la_from[x] <- NULL
      out <- as.character(out)
    }
  })
  result[, (conv_back_ids) := result_conv]
  rm(result_conv, factor_la_from, factor_le_from, factor_la_from_time, factor_le_from_time)

  if(!is.null(add_column)) {
    result_conv_add <- lapply(add_column, function(x) {
      if(length(is.na(result[[x]]))==0 | all(is.na(result[[x]]))) {
        out <- as.character(result[[x]])
      } else {
        out <- factor(result[[x]],
                      labels = as.character(unlist(factor_la_to[x])),
                      levels = 1:factor_le_to[x])
        factor_la_to[x] <- NULL
        out <- as.character(out)
      }
    })
    result[, (add_column) := result_conv_add]
    rm(result_conv_add)
  }
  rm(factor_la_to, factor_le_to, factor_la_to_time, factor_le_to_time)

  return(result)
}

