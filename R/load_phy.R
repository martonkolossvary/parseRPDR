#' @title Loads helath history information into R.
#' @export
#'
#' @description Loads vital signs, social history, immunizations, and various other health history details into the R environment.
#'
#' @param file string, full file path to Phy.txt.
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
#' @return data table, with health history information.
#' \describe{
#'  \item{ID_MERGE}{numeric, defined IDs by \emph{merge_id}, used for merging later.}
#'  \item{ID_phy_EMPI}{string, Unique Partners-wide identifier assigned to the patient used to consolidate patient information
#'  from \emph{phy} datasource, corresponds to EMPI in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_phy_PMRN}{string, Epic medical record number. This value is unique across Epic instances within the Partners network
#'  from \emph{phy} datasource, corresponds to EPIC_PMRN in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_phy_loc}{string, if mrn_type == TRUE, then the data in \emph{MRN_Type} and \emph{MRN} are parsed into IDs corresponding to locations \emph{(loc)}. Data is formatted using pretty_mrn().}
#'  \item{time_phy}{POSIXct, Date when the diagnosis was noted, corresponds to Date in RPDR. Converted to POSIXct format.}
#'  \item{phy_name}{string, Type of clinical value/observation recorded, corresponds to Concept_Name in RPDR.}
#'  \item{phy_code}{string, Source-specific identifier for the specific type of clinical observation, corresponds to Code in RPDR.}
#'  \item{phy_code_type}{string, Source system for the value, corresponds to Code_type in RPDR.}
#'  \item{phy_result}{string, Value associated with the clinical observation. Note: BMI results are calculated internally in the RPDR, corresponds to Results in RPDR.}
#'  \item{phy_unit}{string, Units associated with the clinical observation, corresponds to Units in RPDR.}
#'  \item{phy_provider}{string, Provider of record for the encounter where the observation was recorded, corresponds to Providers in RPDR.}
#'  \item{phy_clinic}{string, Specific department/location where the patient observation was recorded, corresponds to Clinic in RPDR.}
#'  \item{phy_hosp}{string, Facility where the observation was recorded, corresponds to Hospital in RPDR.}
#'  \item{phy_inpatient}{string, Classifies the type of encounter where the observation was entered, corresponds to Inpatient_Outpatient in RPDR.}
#'  \item{phy_enc_num}{string, Unique identifier of the record/visit. This values includes the source system and a unique identifier within the source system, corresponds to Encounter_number in RPDR.}
#'  }
#'
#' @encoding UTF-8
#'
#' @examples \dontrun{
#' #Using defaults
#' d_phy <- load_phy(file = "test_Phy.txt")
#'
#' #Use sequential processing
#' d_phy <- load_phy(file = "test_Phy.txt", nThread = 1)
#'
#' #Use parallel processing and parse data in MRN_Type and MRN columns and keep all IDs
#' d_phy <- load_phy(file = "test_Phy.txt", nThread = 20, mrn_type = TRUE, perc = 1)
#' }

load_phy <- function(file, merge_id = "EMPI", sep = ":", id_length = "standard", perc = 0.6, na = TRUE, identical = TRUE, nThread = parallel::detectCores()-1, mrn_type = FALSE) {

  DATA <- load_base(file = file, merge_id = merge_id, sep = sep, id_length = id_length, perc = perc, na = na, identical = identical, nThread = nThread, mrn_type = mrn_type, src = "phy")
  raw_id <- which(colnames(DATA) == "EMPI" | colnames(DATA) == "IncomingId")[1]
  data_raw <- DATA[, raw_id:dim(DATA)[2]]
  DATA     <- DATA[, 1:(raw_id-1)]

  #Add additional information
  DATA$time_phy       <- as.POSIXct(data_raw$Date, format = "%m/%d/%Y")
  DATA$phy_name       <- pretty_text(data_raw$Concept_Name)
  DATA$phy_code       <- pretty_text(data_raw$Code)
  DATA$phy_code_type  <- pretty_text(data_raw$Code_Type)
  DATA$phy_result     <- pretty_text(data_raw$Result)
  DATA$phy_unit       <- pretty_text(data_raw$Units)
  DATA$phy_provider   <- pretty_text(data_raw$Provider)
  DATA$phy_clinic     <- pretty_text(data_raw$Clinic)
  DATA$phy_hosp       <- pretty_text(data_raw$Hospital)
  DATA$phy_inpatient  <- pretty_text(data_raw$Inpatient_Outpatient)
  DATA$phy_enc_num    <- pretty_text(data_raw$Encounter_number)

  if(dim(DATA)[1] != 1) {DATA <- remove_column(dt = DATA, na = na, identical = identical)}
  return(DATA)
}
