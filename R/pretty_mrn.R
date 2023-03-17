#' @title Converts MRN integer to string compatible with RPDR.
#' @export
#'
#' @description Adds or removes zeros from integers to comply with MRN code standards for given institution and adds institution prefix.
#'
#' @param v vector, integer or sting vector with MRNs.
#' @param prefix string or vector, hospital ID from where the MRNs are from. Defaults to \emph{MGH}. If a vector is provided then it must be the same length as \emph{v}.
#' This allows to potentially use different prefixes for different IDs using the same vector of values.
#' @param sep string, divider between hospital ID and MRN. Defaults to \emph{:}.
#' @param id_length string, indicating whether to modify MRN length based-on required values \emph{id_length = standard}, or to keep lengths as is \emph{id_length = asis}.
#' If \emph{id_length = standard} then in case of \emph{MGH, BWH, MCL, EMPI and PMRN} the length of the MRNs are corrected accordingly by adding zeros, or removing numeral from the beginning.
#' In other cases the lengths are unchanged. Defaults to \emph{standard}.
#' @param nThread integer, number of threads to use by \emph{dopar} for parallelization. If it is set to 1, then no parallel backends are created and the function is executed sequentially.
#'
#' @return vector, with characters formatted to specified lengths. If length of the ID does not match the required length, then leading zeros are added to the ID.
#' If the ID is longer then the required length, then numerals from the beginning of the ID are cut off until it is the required length.
#'
#' @encoding UTF-8
#'
#' @examples \dontrun{
#' mrns <- sample(1e4:1e7, size = 10) #Simulate MRNs
#'
#' #MGH format
#' pretty_mrn(v = mrns, prefix = "MGH")
#'
#' #BWH format
#' pretty_mrn(v = mrns, prefix = "BWH")
#'
#' #Multiple sources using space as a separator
#' pretty_mrn(v = mrns[1:3], prefix = c("MGH", "BWH", "EMPI"), sep = " ")
#'
#' #Keeping the length of the IDs despite not adhering to the requirements
#' pretty_mrn(v = mrns, prefix = "EMPI", id_length = "asis")
#' }

pretty_mrn <- function(v, prefix = "MGH", sep = ":", id_length = "standard", nThread = 1) {

  .SD=.N=.I=.GRP=.BY=.EACHI=..=..cols=.SDcols=i=j=time_to_db=..which_ids_to=..which_ids_from <- NULL

  #Initialize multicore
  if(nThread == 1) {
    `%exec%` <- foreach::`%do%`
  } else {
    cl <- parallel::makeCluster(nThread, methods = FALSE, useXDR = FALSE, )
    doParallel::registerDoParallel(cl)
    `%exec%` <- foreach::`%dopar%`
  }

  #Initiate chunks
  v_split <- split(v, sort(rep_len(1:nThread, length(v))))

  result <- foreach::foreach(i = 1:nThread, .combine="c",
                             .inorder=TRUE,
                             .errorhandling = c("pass"), .verbose=FALSE) %exec%
    {
      v_i <- v_split[[i]]

      #Create vectors of same length
      if(length(prefix) == 1) {
        prefix <- rep(prefix, length(v_i))
      }

      #Create length vector
      length <- len <- nchar(as.character(v_i))

      if(id_length == "standard") {
        length[prefix == "MGH"] <- 7
        length[prefix == "BWH"] <- 8
        length[prefix == "MCL"] <- 6
        length[prefix == "EMPI"] <- 9
        length[prefix == "PMRN"] <- 11
      }

      dif <- length - len
      v_out <- rep(NA, length(v_i))

      for(j in  1:length(v_i)) {
        if(!is.na(v_i[j])) {
          if(dif[j]<0) {
            v_out[j] <- paste0(prefix[j], sep, sub(paste0(rep(".", abs(dif[j])), collapse = ""), "", v_i[j])) #If string is longer than anticipated
          } else{
            v_out[j] <- paste0(prefix[j], sep, strrep("0", dif[j]), v_i[j]) #If string is shorter or equal than anticipated
          }
        }
      }
      v_out
    }

  if(exists("cl") & nThread>1) {parallel::stopCluster(cl)}
  return(result)
}
