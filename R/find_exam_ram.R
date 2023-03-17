#' @title Find exam data within a given timeframe using parallel CPU computing without shared RAM management.
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

find_exam_ram <- function(d_from, d_to,
                          d_from_ID = "ID_MERGE", d_to_ID = "ID_MERGE",
                          d_from_time = "time_rad_exam", d_to_time = "time_enc_admit",
                          time_diff_name = "timediff_exam_to_db", before = TRUE, after = TRUE, time = 1, time_unit = "days",
                          multiple = "closest", add_column = NULL, keep_data = FALSE, nThread = parallel::detectCores()-1) {

  .SD=.N=.I=.GRP=.BY=.EACHI=..=..cols=.SDcols=i=j=time_to_db=..which_ids_to=..which_ids_from=..collapse <- NULL

  #Initialize multicore
  if(nThread == 1) {
    `%exec%` <- foreach::`%do%`
  } else {
    cl <- parallel::makeCluster(nThread, methods = FALSE, useXDR = FALSE)
    doParallel::registerDoParallel(cl)
    `%exec%` <- foreach::`%dopar%`
  }

  #Initiate output
  empty <- d_from[FALSE, ]
  empty[, (time_diff_name) := difftime(NULL, NULL, units = time_unit)]
  empty[, time_to_db := as.POSIXct(NULL)]
  if (!is.null(add_column)) {empty[, (add_column):=character()]}
  out <- empty; i = 1

  #Create iterator
  if(nThread == 1 | dim(d_to)[1]<100) {
    blocks <- list(1:dim(d_to)[1])
  } else {
    groups <- cut(1:dim(d_to)[1], breaks = nThread, labels = 1:nThread)
    ids    <- 1:dim(d_to)[1]
    blocks <- split(ids, groups)
  }

  message(paste0("Finding ", multiple, " data within ", time, " ", time_unit, "."))

  result <- foreach::foreach(j = 1:length(blocks), .combine="rbind",
                             .inorder=TRUE,
                             .errorhandling = c("pass"), .verbose=FALSE) %exec%
    {
      get_ids <- blocks[[j]]
      Exams <- NULL

      for(i in get_ids) {
        Exam_i <- d_from[get(d_from_ID) == d_to[i, get(d_to_ID)]]

        if(dim(Exam_i)[1] != 0) {

          #Calculate time differences
          dif_i <- difftime(trunc.POSIXt(Exam_i[, get(d_from_time)], units = time_unit),
                            trunc.POSIXt(d_to[, get(d_to_time)][i], units = time_unit), units = time_unit)


          #Filter if before or after index event
          if(!after) {
            dif_i[dif_i > 0] <- NA
          } else if(!before) {
            dif_i[dif_i < 0] <- NA
          }
          dif_i[abs(dif_i) > time] <- NA #Get only within time window

          #Add time differences and the time in d_to which was used for pairing
          Exam_i[[time_diff_name]] <- dif_i
          Exam_i[["time_to_db"]] <- d_to[, get(d_to_time)][i]

          #If to add a column from the database used for reference
          if(!is.null(add_column)) {
            Exam_i[[add_column]] <- d_to[, get(add_column)][i]
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
              empty_i[[add_column]] <- d_to[, get(add_column)][i]
            }
            empty_i[[d_from_ID]] <- Exam_i[1, get(d_from_ID)]
            Exams_i_sel <- empty_i
          }
          Exams <- rbind(Exams, Exams_i_sel)
        }
      }
      rm(Exam_i, dif_i, Exams_i_sel)
      Exams
    }

  if(exists("cl") & nThread>1) {parallel::stopCluster(cl)}
  return(result)
}

