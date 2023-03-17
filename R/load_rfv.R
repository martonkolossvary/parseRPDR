#' @title Loads reason for visit data into R.
#' @export
#'
#' @description Loads reason for visit information into the R environment.
#'
#' @param file string, full file path to Rfv.txt.
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
#' @return data table, with reason for visit information.
#' \describe{
#'  \item{ID_MERGE}{numeric, defined IDs by \emph{merge_id}, used for merging later.}
#'  \item{ID_rfv_EMPI}{string, Unique Partners-wide identifier assigned to the patient used to consolidate patient information
#'  from \emph{dia} datasource, corresponds to EMPI in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_rfv_PMRN}{string, Epic medical record number. This value is unique across Epic instances within the Partners network
#'  from \emph{rfv} datasource, corresponds to EPIC_PMRN in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_rfv_loc}{string, if mrn_type == TRUE, then the data in \emph{MRN_Type} and \emph{MRN} are parsed into IDs corresponding to locations \emph{(loc)}. Data is formatted using pretty_mrn().}
#'  \item{time_rfv_start}{POSIXct, Start date of the encounter, corresponds to Start_Date in RPDR. Converted to POSIXct format.}
#'  \item{time_rfv_end}{POSIXct, End date of the encounter, corresponds to End_Date in RPDR. Converted to POSIXct format.}
#'  \item{rfv_provider}{string, Primary provider for the encounter, corresponds to Provider in RPDR.}
#'  \item{rfv_hosp}{string, Facility where the encounter occurred, corresponds to Hospital in RPDR.}
#'  \item{rfv_clinic}{string, Specific department/location where the patient encounter took place, corresponds to Clinic in RPDR.}
#'  \item{rfv_chief_complaint}{string, Description of the chief complaint/reason for visit, corresponds to Chief_Complaint in RPDR.}
#'  \item{rfv_concept_id}{string, Epic identifier for the chief complaint/reason for visit, corresponds to Concept_id in RPDR.}
#'  \item{rfv_comment}{string, Free-text comments regarding the chief complain/reason for visit, corresponds to Comments in RPDR.}
#'  \item{rfv_enc_numb}{string, Unique identifier of the record/visit. This values includes the source system, hospital, and a unique identifier within the source system, corresponds to Encounter_number in RPDR.}
#'  }
#'
#' @encoding UTF-8
#'
#' @examples \dontrun{
#' #Using defaults
#' d_rfv <- load_rfv(file = "test_Rfv.txt")
#'
#' #Use sequential processing
#' d_rfv <- load_rfv(file = "test_Rfv.txt", nThread = 1)
#'
#' #Use parallel processing and parse data in MRN_Type and MRN columns and keep all IDs
#' d_rfv <- load_rfv(file = "test_Rfv.txt", nThread = 20, mrn_type = TRUE, perc = 1)
#' }

load_rfv <- function(file, merge_id = "EMPI", sep = ":", id_length = "standard", perc = 0.6, na = TRUE, identical = TRUE, nThread = parallel::detectCores()-1, mrn_type = FALSE) {

  DATA <- load_base(file = file, merge_id = merge_id, sep = sep, id_length = id_length, perc = perc, na = na, identical = identical, nThread = nThread, mrn_type = mrn_type, src = "rfv")
  raw_id <- which(colnames(DATA) == "EMPI" | colnames(DATA) == "IncomingId")[1]
  data_raw <- DATA[, raw_id:dim(DATA)[2]]
  DATA     <- DATA[, 1:(raw_id-1)]

  #Add additional information
  DATA$time_rfv_start       <- as.POSIXct(data_raw$Start_date, format = "%m/%d/%Y %I:%M:%S %p")
  DATA$time_rfv_end         <- as.POSIXct(data_raw$End_date, format = "%m/%d/%Y %I:%M:%S %p")
  DATA$rfv_provider         <- pretty_text(data_raw$Provider)
  DATA$rfv_clinic           <- pretty_text(data_raw$Clinic)
  DATA$rfv_hosp             <- pretty_text(data_raw$Hospital)
  DATA$rfv_chief_complaint  <- pretty_text(data_raw$Chief_complaint)
  DATA$rfv_concept_id       <- pretty_text(data_raw$Concept_id)
  DATA$rfv_comment          <- pretty_text(data_raw$Comments)
  DATA$rfv_enc_num          <- pretty_text(data_raw$Encounter_number)

  if(dim(DATA)[1] != 1) {DATA <- remove_column(dt = DATA, na = na, identical = identical)}
  return(DATA)
}
