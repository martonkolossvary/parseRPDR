#' @title Loads LMR note documents into R.
#' @export
#'
#' @description Loads notes from the LMR legacy EHR system.
#'
#' @param file string, full file path to Lno.txt.
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
#' @return data table, with LMR notes information.
#' \describe{
#'  \item{ID_MERGE}{numeric, defined IDs by \emph{merge_id}, used for merging later.}
#'  \item{ID_lno_EMPI}{string, Unique Partners-wide identifier assigned to the patient used to consolidate patient information
#'  from \emph{lno} datasource, corresponds to EMPI in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_lno_PMRN}{string, Epic medical record number. This value is unique across Epic instances within the Partners network
#'  from \emph{lno} datasource, corresponds to EPIC_PMRN in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_lno_loc}{string, if mrn_type == TRUE, then the data in \emph{MRN_Type} and \emph{MRN} are parsed into IDs corresponding to locations \emph{(loc)}. Data is formatted using pretty_mrn().}
#'  \item{time_lno}{POSIXct, Date when the report was filed, corresponds to LMRNote_Date in RPDR. Converted to POSIXct format.}
#'  \item{lno_rec_id}{string, Internal identifier for this report within the LMR system, corresponds to Record_Id in RPDR.}
#'  \item{lno_status}{string, Completion status of the note, corresponds to Status in RPDR.}
#'  \item{lno_author}{string, Name of user who created the note, corresponds to Author in RPDR.}
#'  \item{lno_author_mrn}{string, Author's user identifier within the LMR system, corresponds to Author_MRN in RPDR.}
#'  \item{lno_COD}{string, Hospital-specific user code of the note author. The first character is a hospital-specific prefix, corresponds to COD in RPDR.}
#'  \item{lno_hosp}{string, Facility where the encounter occurred, corresponds to Institution in RPDR.}
#'  \item{lno_subject}{string, Type of note. This value is derived from the "Subject" line of the narrative text, corresponds to Subject in RPDR.}
#'  \item{lno_rep_txt}{string, Full narrative text of the note, corresponds to Comments in RPDR.}
#'  }
#'
#' @encoding UTF-8
#'
#' @examples \dontrun{
#' #Using defaults
#' d_lno <- load_lno(file = "test_Lno.txt")
#'
#' #Use sequential processing
#' d_lno <- load_lno(file = "test_Lno.txt", nThread = 1)
#'
#' #Use parallel processing and parse data in MRN_Type and MRN columns and keep all IDs
#' d_lno <- load_lno(file = "test_Lno.txt", nThread = 20, mrn_type = TRUE, perc = 1)
#' }

load_lno <- function(file, merge_id = "EMPI", sep = ":", id_length = "standard", perc = 0.6, na = TRUE, identical = TRUE, nThread = parallel::detectCores()-1, mrn_type = FALSE) {

  message(paste("Modifing LMR notes file to be compatible with data.table. Could take considerable time, please be patient!"))

  message("Loading data")
  header <- readr::read_lines(file = file, skip = 0, skip_empty_rows = T, n_max = 1)
  record <- readr::read_lines(file = file, skip = 1, skip_empty_rows = T)
  message("Removing unnecessary carriage returns")
  record <- gsub("[\r\n]", " ", record) #remove carriage returns and new lines
  message("Removing multuiple spaces")
  record <- gsub("^ *|(?<= ) | *$", "", record, perl = TRUE) #remove multiple spaces
  message("Creating proper representation of records")
  record <- gsub(pattern = "[report_end]", replacement = "[report_end]\r\n", x = record,  fixed = TRUE) #add new line to all other rows

  message("Converting texts to data.table compatible format")
  has_end <- which(grepl(pattern = "[report_end]", x = record,  fixed = TRUE))
  batch <- ifelse(length(has_end)<100, length(has_end), 100)
  which_rows <- split(has_end, sort(has_end%%batch)) #split into 100 tables to overcome memory issues

  texts <- lapply(1:batch, function(x) {
    if(x == 1) {
      out <- paste(header, paste(record[1:max(which_rows[[x]])], collapse = " "), sep = "\r\n")
    } else {
      out <- paste(header, paste(record[(max(which_rows[[x-1]])+1):max(which_rows[[x]])], collapse = " "), sep = "\r\n")
    }
  })
  rm(list = c("header", "record", "has_end", "which_rows", "batch"))

  message("Creating data.table")
  #Supply modified text to load_base function and continue as other load functions
  DATA <- lapply(texts, function(x){
    suppressMessages(load_base(file = x, merge_id = merge_id, sep = sep, id_length = id_length, perc = perc, na = na, identical = identical, nThread = 1, mrn_type = mrn_type, src = "lno"))
  })
  rm(list = c("texts"))
  DATA <- data.table::rbindlist(DATA)
  raw_id <- which(colnames(DATA) == "EMPI" | colnames(DATA) == "IncomingId")[1]
  data_raw <- DATA[, raw_id:dim(DATA)[2]]
  DATA     <- DATA[, 1:(raw_id-1)]

  #Add additional information
  DATA$time_lno <- as.POSIXct(data_raw$LMRNote_Date, format = "%m/%d/%Y %I:%M:%S %p")
  DATA$lno_rec_id <- pretty_text(data_raw$Record_Id)
  DATA$lno_status <- pretty_text(data_raw$Status)
  DATA$lno_author <- pretty_text(data_raw$Author)
  DATA$lno_author_mrn <- pretty_text(data_raw$Author_MRN)
  DATA$lno_COD  <- pretty_text(data_raw$COD)
  DATA$lno_hosp <- pretty_text(data_raw$Institution)
  DATA$lno_subject <- pretty_text(data_raw$Subject)
  DATA$lno_rep_txt <- data_raw$Comments

  if(dim(DATA)[1] != 1) {DATA <- remove_column(dt = DATA, na = na, identical = identical)}
  return(DATA)
}
