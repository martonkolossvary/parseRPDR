#' @title Loads note documents into R.
#' @export
#'
#' @description Loads documents information into the R environment, which are:
#' \describe{
#' \item{Cardiology: }{"car"}
#' \item{Discharge: }{"dis"}
#' \item{Endoscopy: }{"end"}
#' \item{History & Physical: }{"hnp"}
#' \item{Operative: }{"opn"}
#' \item{Pathology: }{"pat"}
#' \item{Progress: }{"prg"}
#' \item{Pulmonary: }{"pul"}
#' \item{Radiology: }{"rad"}
#' \item{Visit: }{"vis"}
#' }
#'
#' @param file string, full file path to given type of note i.e. Hnp.txt.
#' @param type string, the type of note to be loaded. May be on of: "car", "dis", "end", "hnp", "opn", "pat", "prg", "pul", "rad" or "vis".
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
#' @param load_report boolean, should the report text be returned in the data table. Defaults to \emph{TRUE}. However, be aware that some notes may take up more memory than available on the machine.
#' @param format_orig boolean, should report be returned in its original formatting or should white spaces used for formatting be removed. Defaults to \emph{FALSE}.
#'
#' @return data table, with notes information. \emph{abc} stands for the three letter abbreviation of the given type of note.
#' \describe{
#'  \item{ID_MERGE}{numeric, defined IDs by \emph{merge_id}, used for merging later.}
#'  \item{ID_abc_EMPI}{string, Unique Partners-wide identifier assigned to the patient used to consolidate patient information
#'  from \emph{abc} datasource, corresponds to EMPI in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_abc_PMRN}{string, Epic medical record number. This value is unique across Epic instances within the Partners network
#'  from \emph{abc} datasource, corresponds to EPIC_PMRN in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_abc_loc}{string, if mrn_type == TRUE, then the data in \emph{MRN_Type} and \emph{MRN} are parsed into IDs corresponding to locations \emph{(loc)}. Data is formatted using pretty_mrn().}
#'  \item{abc_rep_num}{string, Source-specific identifier used to reference the report, corresponds to Report_Number in RPDR.}
#'  \item{time_abc}{POSIXct, Date when the report was filed, corresponds to Report_Date_Time in RPDR. Converted to POSIXct format.}
#'  \item{abc_rep_desc}{string, Type of report or procedure documented in the report, corresponds to Report_Description in RPDR.}
#'  \item{abc_rep_status}{string, Completion status of the note/report, corresponds to Report_Status in RPDR.}
#'  \item{abc_rep_type}{string, See specification in RPDR data dictionary, corresponds to Report_Type in RPDR.}
#'  \item{abc_rep_txt}{string, Full narrative text contained in the note/report, corresponds to Report_Text in RPDR. Only provided if \emph{load_report} is TRUE.}
#'  }
#'
#' @encoding UTF-8
#'
#' @examples \dontrun{
#' #Using defaults
#' d_hnp <- load_notes(file = "test_Hnp.txt", type = "hnp")
#'
#' #Use sequential processing
#' d_hnp <- load_notes(file = "test_Hnp.txt", type = "hnp", nThread = 1, format_orig = TRUE)
#'
#' #Use parallel processing and parse data in MRN_Type and MRN columns and keep all IDs
#' d_hnp <- load_notes(file = "test_Hnp.txt", type = "hnp", nThread = 20, mrn_type = TRUE, perc = 1)
#' }

load_notes <- function(file, type, merge_id = "EMPI", sep = ":", id_length = "standard", perc = 0.6, na = TRUE, identical = TRUE, nThread = parallel::detectCores()-1, mrn_type = FALSE, load_report = TRUE, format_orig = FALSE) {

  supp <- c("car", "dis", "end", "hnp", "opn", "pat", "prg", "pul", "rad", "vis")
  if(!(type %in% supp)) {stop("type argument must be one of: ", paste0(supp, collapse = ", "))}
  #Modify txt to be compatible with data.table
  message(paste("Modifing ", type, " notes file to be compatible with data.table. Could take considerable time, please be patient!"))

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
    suppressMessages(load_base(file = x, merge_id = merge_id, sep = sep, id_length = id_length, perc = perc, na = na, identical = identical, nThread = 1, mrn_type = mrn_type, src = type))
  })
  rm(list = c("texts"))
  DATA <- data.table::rbindlist(DATA)
  raw_id <- which(colnames(DATA) == "EMPI" | colnames(DATA) == "IncomingId")[1]
  data_raw <- DATA[, raw_id:dim(DATA)[2]]
  DATA     <- DATA[, 1:(raw_id-1)]

  #Add additional information
  DATA[[paste0("time_", type)]]        <- as.POSIXct(data_raw$Report_Date_Time, format = "%m/%d/%Y %I:%M:%S %p")
  DATA[[paste0(type, "_rep_num")]]     <- pretty_text(data_raw$Report_Number)
  DATA[[paste0(type, "_rep_desc")]]    <- pretty_text(data_raw$Report_Description)
  DATA[[paste0(type, "_rep_status")]]  <- pretty_text(data_raw$Report_Status)
  DATA[[paste0(type, "_rep_type")]]    <- pretty_text(data_raw$Report_Type)
  if(load_report) {
    DATA[[paste0(type, "_rep_txt")]] <- data_raw$Report_Text
    if(format_orig) {
      DATA[[paste0(type, "_rep_txt")]] <- gsub(pattern = "~~~~~", replacement = "\r\n",
                                               x = DATA[[paste0(type, "_rep_txt")]], fixed = TRUE)
    }
  }

  if(dim(DATA)[1] != 1) {DATA <- remove_column(dt = DATA, na = na, identical = identical)}
  return(DATA)
}
