#' @title Loads microbiology results into R.
#' @export
#'
#' @description Loads microbiology results into the R environment.
#'
#' @param file string, full file path to Mic.txt
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
#' @param format_orig boolean, should report be returned in its original formatting or should white spaces used for formatting be removed. Defaults to \emph{FALSE}.
#'
#' @return data table, with microbiology information.
#' \describe{
#'  \item{ID_MERGE}{numeric, defined IDs by \emph{merge_id}, used for merging later.}
#'  \item{ID_mic_EMPI}{string, Unique Partners-wide identifier assigned to the patient used to consolidate patient information
#'  from \emph{mic} datasource, corresponds to EMPI in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_mic_PMRN}{string, Epic medical record number. This value is unique across Epic instances within the Partners network
#'  from \emph{mic} datasource, corresponds to EPIC_PMRN in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_mic_loc}{string, if mrn_type == TRUE, then the data in \emph{MRN_Type} and \emph{MRN} are parsed into IDs corresponding to locations \emph{(loc)}. Data is formatted using pretty_mrn().}
#'  \item{time_mic}{POSIXct, Date when the specimen was received by the laboratory, corresponds to Microbiology_Date_Time in RPDR. Converted to POSIXct format.}
#'  \item{mic_org_code}{string, Internal identifier for the organism used by the source system, corresponds to Organism_Code in RPDR.}
#'  \item{mic_org_name}{string, Name of the organism identified or tested, corresponds to Organism_Name in RPDR.}
#'  \item{mic_org_text}{string, Full narrative text of the test and results, including sensitivities, corresponds to Organism_Text in RPDR.}
#'  \item{mic_org_comment}{string, Free-text information about the organism or result, corresponds to Organism_Comment in RPDR.}
#'  \item{mic_test_code}{string, Internal identifier for the test used by the source system, corresponds to Test_Code in RPDR.}
#'  \item{mic_test_name}{string, Name of the assay to be performed, or the results of a culture, corresponds to Test_Name in RPDR.}
#'  \item{mic_test_status}{string, Status of the results, i.e. preliminary or final, corresponds to Test_Status in RPDR.}
#'  \item{mic_test_comment}{string, Free-text information about the test and results, corresponds to Test_Comments in RPDR.}
#'  \item{mic_spec}{string, Type of specimen collected to perform the test, corresponds to Specimen_Type in RPDR.}
#'  \item{mic_spec_txt}{string, Free-text information about the specimen, its collection or its integrity, corresponds to Specimen_Comments in RPDR.}
#'  \item{mic_accession}{string, Internal tracking number assigned to the specimen for identification in the microbiology lab, corresponds to Microbiology_Number in RPDR.}
#'  }
#'
#' @encoding UTF-8
#'
#' @examples \dontrun{
#' #Using defaults
#' d_mic <- load_mic(file = "test_Mic.txt")
#'
#' #Use sequential processing
#' d_mic <- load_mic(file = "test_Mic.txt", nThread = 1)
#'
#' #Use parallel processing and parse data in MRN_Type and MRN columns and keep all IDs
#' d_mic <- load_mic(file = "test_Mic.txt", nThread = 20, mrn_type = TRUE, perc = 1)
#' }

load_mic <- function(file, merge_id = "EMPI", sep = ":", id_length = "standard", perc = 0.6, na = TRUE, identical = TRUE, nThread = parallel::detectCores()-1, mrn_type = FALSE, format_orig = FALSE) {

  message(paste("Modifing microbiology table to be compatible with data.table. Could take considerable time, please be patient!"))

  message("Loading data")
  header <- readr::read_lines(file = file, skip = 0, skip_empty_rows = T, n_max = 1)
  record <- readr::read_lines(file = file, skip = 1, skip_empty_rows = T)
  if(!format_orig) {
    message("Removing unnecessary carriage returns")
    record <- gsub("[\r\n]", " ", record) #remove carriage returns and new lines
    message("Removing multiple spaces")
    record <- gsub("^ *|(?<= ) | *$", "", record, perl = TRUE) #remove multiple spaces
  }

  message("Creating proper representation of records")
  record <- gsub(pattern = "[report_end]", replacement = "[report_end]\r\n", x = record,  fixed = TRUE) #add new line to all other rows

  message("Converting texts to data.table compatible format")
  has_end <- which(grepl(pattern = "[report_end]", x = record,  fixed = TRUE))
  batch <- ifelse(length(has_end)<100, length(has_end), 100)

  split_IDs  <- suppressWarnings(split(1:length(has_end), 1:batch))
  split_IDs  <- lapply(1:length(split_IDs), function(x) {rep(x, length(split_IDs[[x]]))})
  which_rows <- suppressWarnings(split(has_end, unlist(split_IDs))) #split into batch number of tables to overcome memory issues

  texts <- lapply(1:batch, function(x) {
    if(x == 1) {
      if(format_orig) {
        out <- paste(header, paste(record[1:max(which_rows[[x]])], collapse = "~~~~~"), sep = "\r\n")
      } else {
        out <- paste(header, paste(record[1:max(which_rows[[x]])], collapse = " "), sep = "\r\n")
      }
    } else {
      if(format_orig) {
        out <- paste(header, paste(record[(max(which_rows[[x-1]])+1):max(which_rows[[x]])], collapse = "~~~~~"), sep = "\r\n")
      } else {
        out <- paste(header, paste(record[(max(which_rows[[x-1]])+1):max(which_rows[[x]])], collapse = " "), sep = "\r\n")
      }
    }
  })
  rm(list = c("header", "record", "has_end", "which_rows", "batch", "split_IDs"))

  message("Creating data.table")
  #Supply modified text to load_base function and continue as other load functions
  DATA <- lapply(texts, function(x){
    suppressMessages(load_base(file = x, merge_id = merge_id, sep = sep, id_length = id_length, perc = perc, na = na, identical = identical, nThread = 1, mrn_type = mrn_type, src = "mic"))
  })
  rm(list = c("texts"))
  DATA <- data.table::rbindlist(DATA)
  raw_id <- which(colnames(DATA) == "EMPI" | colnames(DATA) == "IncomingId")[1]
  data_raw <- DATA[, raw_id:dim(DATA)[2]]
  DATA     <- DATA[, 1:(raw_id-1)]

  #Add additional information
  DATA$time_mic         <- as.POSIXct(data_raw$Microbiology_Date_Time, format = "%m/%d/%Y %H:%M")
  DATA$mic_org_code       <- pretty_text(data_raw$Organism_Code)
  DATA$mic_org_name       <- pretty_text(data_raw$Organism_Name)
  DATA$mic_org_text       <- pretty_text(data_raw$Organism_Text)
  DATA$mic_org_comment    <- pretty_text(data_raw$Organism_Comment)
  DATA$mic_test_code       <- pretty_text(data_raw$Test_Code)
  DATA$mic_test_name       <- pretty_text(data_raw$Test_Name)
  DATA$mic_test_status     <- pretty_text(data_raw$Test_Status)
  DATA$mic_test_comment    <- pretty_text(data_raw$Test_Comments)
  DATA$mic_spec         <- pretty_text(data_raw$Specimen_Type)
  DATA$mic_spec_txt     <- pretty_text(data_raw$Specimen_Comments)
  DATA$mic_accession    <- pretty_text(data_raw$Microbiology_Number)

  if(dim(DATA)[1] != 1) {DATA <- remove_column(dt = DATA, na = na, identical = identical)}
  return(DATA)
}
