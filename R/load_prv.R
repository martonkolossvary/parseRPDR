#' @title Loads providers information into R.
#' @export
#'
#' @description Loads providers information into the R environment.
#'
#' @param file string, full file path to Prv.txt.
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
#' @param mrn_type boolean, should data in \emph{MRN_Type} and \emph{MRN} be parsed. Defaults to \emph{TURE} only for Con.txt, as it is not advised to parse these for all data sources as it takes considerable time.
#'
#' @return data table, with provider information data.
#' \describe{
#'  \item{ID_MERGE}{numeric, defined IDs by \emph{merge_id}, used for merging later.}
#'  \item{ID_con_EMPI}{string, Unique Partners-wide identifier assigned to the patient used to consolidate patient information
#'  from \emph{con} datasource, corresponds to EMPI in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_con_PMRN}{string, Epic medical record number. This value is unique across Epic instances within the Partners network
#'  from \emph{con}datasource, corresponds to EPIC_PMRN in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_con_loc}{string, if mrn_type == TRUE, then the data in \emph{MRN_Type} and \emph{MRN} are parsed into IDs corresponding to locations \emph{(loc)}. Data is formatted using pretty_mrn().}
#'  \item{time_prv_last_seen}{POSIXct, Date when the patient was last seen by the provider, corresponds to Last_Seen_Date in RPDR.}
#'  \item{prv_name}{string, Full name of the provider, corresponds to Provider_Name in RPDR.}
#'  \item{prv_rank}{string, Provides a quantitative value of provider's level of interaction with the patient.
#'  This is calculated using the number of CPT codes for face-to-face visits that the provider has billed for in relation to the patient, corresponds to Provider_Rank in RPDR.}
#'  \item{prv_ID}{string, Identification code for the provider, including the source institution, corresponds to Provider_ID in RPDR.}
#'  \item{prv_ID_CMP}{string, Corporate Provider Master ID. This is the unique identifier for a provider across the MGB network, corresponds to CPM_Id in RPDR.}
#'  \item{prv_spec}{string, Comma-delimited list of the provider's specialties, corresponds to Specialties in RPDR.}
#'  \item{prv_pcp}{string, Available for BWH and MGH PCPs only. Flag indicating whether the provider is listed as the
#'  patient's Primary Care Physician, corresponds to Is_PCP in RPDR.}
#'  \item{prv_dep}{string, Provider's department, corresponds to Enterprise_service in RPDR.}
#'  \item{prv_address1}{string, Address of the provider's primary practice, corresponds to Address_1 in RPDR.}
#'  \item{prv_address2}{string, Additional address information, corresponds to Address_2 in RPDR.}
#'  \item{prv_city}{string, City of the provider's primary practice, corresponds to City in RPDR.}
#'  \item{prv_state}{string, State of the provider's primary practice, corresponds to State in RPDR.}
#'  \item{prv_zip}{string, Mailing zip code of provider's primary practice, corresponds to Zip in RPDR.}
#'  \item{prv_phone}{string, Telephone number of the provider's primary practice, corresponds to Phone_Ext in RPDR.}
#'  \item{prv_fax}{string, Fax number of the provider's primary practice, corresponds to Fax in RPDR.}
#'  \item{prv_email}{string, Primary e-mail address for the provider, corresponds to Email in RPDR.}
#'  }
#'
#' @encoding UTF-8
#'
#' @examples \dontrun{
#' #Using defaults
#' d_prv <- load_prv(file = "test_Prv.txt")
#'
#' #Use sequential processing
#' d_prv <- load_prv(file = "test_Prv.txt", nThread = 1)
#'
#' #Use parallel processing and parse data in
#' #MRN_Type and MRN columns (default in load_con) and keep all IDs
#' d_prv <- load_prv(file = "test_Prv.txt", nThread = 20, mrn_type = TRUE, perc = 1)
#' }

load_prv <- function(file, merge_id = "EMPI", sep = ":", id_length = "standard", perc = 0.6, na = TRUE, identical = TRUE, nThread = parallel::detectCores()-1, mrn_type = TRUE) {

  DATA <- load_base(file = file, merge_id = merge_id, sep = sep, id_length = id_length, perc = perc, na = na, identical = identical, nThread = nThread, mrn_type = mrn_type, src = "prv")
  raw_id <- which(colnames(DATA) == "EMPI" | colnames(DATA) == "IncomingId")[1]
  data_raw <- DATA[, raw_id:dim(DATA)[2]]
  DATA     <- DATA[, 1:(raw_id-1)]

  #Add additional information
  DATA$time_prv_last_seen <- as.POSIXct(data_raw$Last_Seen_Date, format = "%m/%d/%Y")
  DATA$prv_name     <- pretty_text(data_raw$Provider_Name)
  DATA$prv_rank     <- pretty_text(data_raw$Provider_Rank)
  DATA$prv_ID       <- pretty_text(data_raw$Provider_ID)
  DATA$prv_ID_CMP   <- pretty_text(data_raw$CPM_Id)
  DATA$prv_spec     <- pretty_text(data_raw$Specialties)
  DATA$prv_pcp      <- pretty_text(data_raw$Is_PCP)
  DATA$prv_dep      <- pretty_text(data_raw$Enterprise_service)
  DATA$prv_address1     <- pretty_text(data_raw$Address_1)
  DATA$prv_address2     <- pretty_text(data_raw$Address_2)
  DATA$prv_city         <- pretty_text(data_raw$City)
  DATA$prv_state        <- pretty_text(data_raw$State)
  DATA$prv_zip          <- pretty_numbers(data_raw$Zip)
  DATA$prv_phone       <- pretty_numbers(data_raw$Phone_Ext, length_final = 10, remove_from_back = NULL)
  DATA$prv_fax         <- pretty_text(data_raw$Fax)
  DATA$prv_email       <- pretty_text(data_raw$Email)

  if(dim(DATA)[1] != 1) {DATA <- remove_column(dt = DATA, na = na, identical = identical)}
  return(DATA)
}

