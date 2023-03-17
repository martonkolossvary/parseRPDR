#' @title Loads patient data information into R.
#' @export
#'
#' @description Loads patient data information into the R environment.
#'
#' @param file string, full file path to Ptd.txt.
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
#' @return data table, with patient data information information.
#' \describe{
#'  \item{ID_MERGE}{numeric, defined IDs by \emph{merge_id}, used for merging later.}
#'  \item{ID_ptd_EMPI}{string, Unique Partners-wide identifier assigned to the patient used to consolidate patient information
#'  from \emph{ptd} datasource, corresponds to EMPI in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_ptd_PMRN}{string, Epic medical record number. This value is unique across Epic instances within the Partners network
#'  from \emph{ptd} datasource, corresponds to EPIC_PMRN in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_ptd_loc}{string, if mrn_type == TRUE, then the data in \emph{MRN_Type} and \emph{MRN} are parsed into IDs corresponding to locations \emph{(loc)}. Data is formatted using pretty_mrn().}
#'  \item{time_ptd_start}{POSIXct, Date item was initiated in the record, corresponds to Start_Date in RPDR. Converted to POSIXct format.}
#'  \item{time_ptd_end}{POSIXct, Date item was finalized in the record, corresponds to End_Date in RPDR. Converted to POSIXct format.}
#'  \item{ptd_desc}{string, Name of the item being reported, corresponds to Description in RPDR.}
#'  \item{ptd_result}{string, Result of the item being reported, corresponds to Result in RPDR.}
#'  \item{ptd_type}{string, Describes the type of data being reported, corresponds to Patient_Data_Type in RPDR.}
#'  \item{ptd_enc_num}{string, Unique identifier of the record/visit. This values includes the source system and a unique identifier within the source system, corresponds to Encounter_number in RPDR.}
#'  }
#'
#' @encoding UTF-8
#'
#' @examples \dontrun{
#' #Using defaults
#' d_ptd <- load_ptd(file = "test_Phy.txt")
#'
#' #Use sequential processing
#' d_ptd <- load_ptd(file = "test_Phy.txt", nThread = 1)
#'
#' #Use parallel processing and parse data in MRN_Type and MRN columns and keep all IDs
#' d_ptd <- load_ptd(file = "test_Phy.txt", nThread = 20, mrn_type = TRUE, perc = 1)
#' }

load_ptd <- function(file, merge_id = "EMPI", sep = ":", id_length = "standard", perc = 0.6, na = TRUE, identical = TRUE, nThread = parallel::detectCores()-1, mrn_type = FALSE) {

  DATA <- load_base(file = file, merge_id = merge_id, sep = sep, id_length = id_length, perc = perc, na = na, identical = identical, nThread = nThread, mrn_type = mrn_type, src = "ptd")
  raw_id <- which(colnames(DATA) == "EMPI" | colnames(DATA) == "IncomingId")[1]
  data_raw <- DATA[, raw_id:dim(DATA)[2]]
  DATA     <- DATA[, 1:(raw_id-1)]

  #Add additional information
  DATA$time_ptd_start <- as.POSIXct(data_raw$Start_Date, format = "%m/%d/%Y %H:%M")
  DATA$time_ptd_end   <- as.POSIXct(data_raw$End_Date, format = "%m/%d/%Y %H:%M")
  DATA$ptd_desc       <- pretty_text(data_raw$Description)
  DATA$ptd_result     <- pretty_text(data_raw$Result)
  DATA$ptd_type       <- pretty_text(data_raw$Patient_Data_Type)
  DATA$ptd_enc_num    <- pretty_text(data_raw$Encounter_ID)

  if(dim(DATA)[1] != 1) {DATA <- remove_column(dt = DATA, na = na, identical = identical)}
  return(DATA)
}
