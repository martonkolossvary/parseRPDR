#' @title Loads procedures into R.
#' @export
#'
#' @description Loads Clinical procedure information into the R environment, both Prc and Pec files.
#'
#' @param file string, full file path to Prc.txt or Pec.txt.
#' @param merge_id string, column name to use to create \emph{ID_MERGE} column used to merge different datasets. Defaults to \emph{EPIC_PMRN},
#' as it is the preferred MRN in the RPDR system.
#' @param sep string, divider between hospital ID and MRN. Defaults to \emph{:}.
#' @param id_length string, indicating whether to modify MRN length based-on required values \emph{id_length = standard}, or to keep lengths as is \emph{id_length = asis}.
#' If \emph{id_length = standard} then in case of \emph{MGH, BWH, MCL, EMPI and PMRN} the length of the MRNs are corrected accordingly by adding zeros, or removing numeral from the beginning.
#' In other cases the lengths are unchanged. Defaults to \emph{standard}.
#' @param perc numeric, a number between 0-1 indicating which parsed ID columns to keep. Data present in \emph{perc x 100\%} of patients are kept.
#' @param na boolean, whether to remove columns with only NA values. Defaults to \emph{TRUE}.
#' @param identical boolean, whether to remove columns with identical values. Defaults to \emph{TRUE}.
#' @param nThread integer, number of threads to use to load data.
#' @param mrn_type boolean, should data in \emph{MRN_Type} and \emph{MRN} be parsed. Defaults to \emph{FALSE}, as it is not advised to parse these for all data sources as it takes considerable time.
#'
#' @return data table, with procedural information.
#' \describe{
#'  \item{ID_MERGE}{numeric, defined IDs by \emph{merge_id}, used for merging later.}
#'  \item{ID_prc_EMPI}{string, Unique Partners-wide identifier assigned to the patient used to consolidate patient information
#'  from \emph{prc} datasource, corresponds to EMPI in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_prc_PMRN}{string, Epic medical record number. This value is unique across Epic instances within the Partners network
#'  from \emph{prc} datasource, corresponds to EPIC_PMRN in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_prc_loc}{string, if mrn_type == TRUE, then the data in \emph{MRN_Type} and \emph{MRN} are parsed into IDs corresponding to locations \emph{(loc)}. Data is formatted using pretty_mrn().}
#'  \item{time_prc}{POSIXct, Date when the procedure was performed, corresponds to Date in RPDR. Converted to POSIXct format.}
#'  \item{prc_name}{string, Name of the procedure or operation performed, corresponds to Procedure_Name in RPDR.}
#'  \item{prc_code}{string, Procedure code associated with the "Code_type" value, corresponds to Code in RPDR.}
#'  \item{prc_code_type}{string, Standardized classification system or custom source value associated with the procedure code, corresponds to Code_type in RPDR.}
#'  \item{prc_flag}{string, Qualifier for the diagnosis, corresponds to Procedure_Flag in RPDR.}
#'  \item{prc_quantity}{string, Number of the procedures that were ordered for this record, corresponds to Quantity in RPDR.}
#'  \item{prc_provider}{string, Provider identifies the health care clinician performing the procedure, corresponds to Provider in RPDR.}
#'  \item{prc_clinic}{string, Specific department/location where the procedure was ordered or performed, corresponds to Clinic in RPDR.}
#'  \item{prc_hosp}{string, Facility where the procedure was ordered or performed, corresponds to Hospital in RPDR.}
#'  \item{prc_inpatient}{string, classifies the type of encounter where the procedure was performed or ordered.}
#'  \item{prc_enc_num}{string, Unique identifier of the record/visit, displayed in the following format: Source System - Institution Number, corresponds to Encounter_number in RPDR.}
#'  }
#'
#' @encoding UTF-8
#'
#' @examples \dontrun{
#' #Using defaults
#' d_prc <- load_prc(file = "test_Prc.txt")
#'
#' #Use sequential processing
#' d_prc <- load_prc(file = "test_Prc.txt", nThread = 1)
#'
#' #Use parallel processing and parse data in MRN_Type and MRN columns and keep all IDs
#' d_pec <- load_prc(file = "test_Pec.txt", nThread = 20, mrn_type = TRUE, perc = 1)
#' }

load_prc <- function(file, merge_id = "EMPI", sep = ":", id_length = "standard", perc = 0.6, na = TRUE, identical = TRUE, nThread = parallel::detectCores()-1, mrn_type = FALSE) {

  message(paste("Checking procedure file to be compatible with data.table. Could take considerable time, please be patient!"))
  message("Loading data")
  header <- readr::read_lines(file = file, skip = 0, skip_empty_rows = T, n_max = 1)
  record <- readr::read_lines(file = file, skip = 1, skip_empty_rows = T)
  message("Removing unnecessary carriage returns")
  n_carr <- stringr::str_count(record, stringr::coll("|", ignore_case = TRUE))
  which_n <- n_carr != 14

  record_bad <- record[which_n] #Bad records
  record     <- record[!which_n] #Good records
  if(length(record_bad) != 0) { #If there are new line characters in the text
    n_carr_bad <- n_carr[which_n] #Bad number of characters
    until      <- cumsum(n_carr_bad) %% 14 == 0 #Find where are the end of lines and merge based-on that
    n_times    <- which(until)
    n_times_2  <- suppressWarnings(c(n_times[1], n_times[-1] - n_times))
    if(length(n_times) > 1) {n_times_2  <- n_times_2[-length(n_times_2)]}
    factor_lev <- unlist(mapply(rep, 1:sum(until), n_times_2))
    record_bad <- tapply(record_bad, factor_lev, FUN= paste, collapse=' ')
  }
  record <- c(record, record_bad)
  suppressWarnings(rm(list = c("n_carr", "which_n", "n_carr_bad", "until", "n_times", "n_times_2", "factor_lev", "record_bad")))

  message("Converting texts to data.table compatible format")
  has_end <- 1:length(record)
  batch <- ifelse(length(has_end)<100, length(has_end), 100)
  which_rows <- split(has_end, sort(has_end%%batch)) #split into 100 tables to overcome memory issues

  texts <- lapply(1:batch, function(x) {
    if(x == 1) {
      out <- paste(header, paste(record[1:max(which_rows[[x]])], collapse = "\r\n"), sep = "\r\n")
    } else {
      out <- paste(header, paste(record[(max(which_rows[[x-1]])+1):max(which_rows[[x]])], collapse = "\r\n"), sep = "\r\n")
    }
  })
  rm(list = c("header", "record", "has_end", "which_rows", "batch"))

  message("Creating data.table")
  #Supply modified text to load_base function and continue as other load functions
  DATA <- lapply(texts, function(x){
    suppressMessages(load_base(file = x, merge_id = merge_id, sep = sep, id_length = id_length, perc = perc, na = na, identical = identical, nThread = 1, mrn_type = mrn_type, src = "prc"))
  })
  rm(list = c("texts"))
  DATA <- data.table::rbindlist(DATA)

  raw_id <- which(colnames(DATA) == "EMPI" | colnames(DATA) == "IncomingId")[1]
  data_raw <- DATA[, raw_id:dim(DATA)[2]]
  DATA     <- DATA[, 1:(raw_id-1)]

  #Add additional information
  DATA$time_prc       <- as.POSIXct(data_raw$Date, format = "%m/%d/%Y")
  DATA$prc_name       <- pretty_text(data_raw$Procedure_Name)
  DATA$prc_code       <- pretty_text(data_raw$Code)
  DATA$prc_code_type  <- pretty_text(data_raw$Code_Type)
  DATA$prc_flag       <- pretty_text(data_raw$Procedure_Flag)
  DATA$prc_quantity   <- pretty_text(data_raw$Quantity)
  DATA$prc_provider   <- pretty_text(data_raw$Provider)
  DATA$prc_clinic     <- pretty_text(data_raw$Clinic)
  DATA$prc_hosp       <- pretty_text(data_raw$Hospital)
  DATA$prc_inpatient  <- pretty_text(data_raw$Inpatient_Outpatient)
  DATA$prc_enc_num    <- pretty_text(data_raw$Encounter_number)

  if(dim(DATA)[1] != 1) {DATA <- remove_column(dt = DATA, na = na, identical = identical)}
  return(DATA)
}
