#' @title Find exam data within a given timeframe using parallel CPU computing without shared RAM management.
#' @keywords internal
#'
#' @description Finds all, earliest or closest examination to a given timepoints using parallel computing. A progress bar is also reported in the terminal to show the progress of the computation.
#' @encoding UTF-8

find_exam_ram <- function(d_from, d_to,
                          d_from_ID = "ID_MERGE", d_to_ID = "ID_MERGE",
                          d_from_time = "time_rad_exam", d_to_time = "time_enc_admit",
                          time_diff_name = "timediff_exam_to_db", before = TRUE, after = TRUE, time = 1, time_unit = "days",
                          multiple = "closest", add_column = NULL, keep_data = FALSE, nThread = parallel::detectCores()-1) {

  .SD=.N=.I=.GRP=.BY=.EACHI=..=..cols=.SDcols=i=j=time_to_db=..which_ids_to=..which_ids_from=..collapse <- NULL
  options(future.globals.maxSize = +Inf)

  #Initialize multicore
  if(nThread == 1) {
    `%exec%` <- foreach::`%do%`
    future::plan(future::sequential)
  } else {
    if(parallelly::supportsMulticore()) {
      future::plan(future::multicore, workers = nThread)
    } else {
      future::plan(future::multisession, workers = nThread)
    }
    `%exec%` <- doFuture::`%dofuture%`
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
  divider <- ifelse(floor(dim(d_to)[1]/1000) == 0, 1, floor(dim(d_to)[1]/1000))
  p <- progressr::progressor(steps = dim(d_to)[1]/divider)

  result <- foreach::foreach(j = 1:length(blocks), .combine="rbind",
                             .inorder=TRUE, .options.future = list(chunk.size = 1.0,
                                                                   packages = c("parseRPDR")),
                             .errorhandling = c("pass"), .verbose=FALSE) %exec%
    {
      get_ids <- blocks[[j]]
      Exams <- NULL

      for(i in get_ids) {
        if(i %% divider == 0) {p(sprintf("i=%g", i))}

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

  on.exit(options(future.globals.maxSize = 1.0 * 1e9))
  future::plan(future::sequential)
  return(result)
}

