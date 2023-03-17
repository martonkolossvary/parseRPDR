#' @title Loads contact information into R.
#' @export
#'
#' @description Loads patient contact, insurance, and PCP information into the R environment.
#'
#' @param file string, full file path to Con.txt.
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
#' @return data table, with contact information data.
#' \describe{
#'  \item{ID_MERGE}{numeric, defined IDs by \emph{merge_id}, used for merging later.}
#'  \item{ID_con_EMPI}{string, Unique Partners-wide identifier assigned to the patient used to consolidate patient information
#'  from \emph{con} datasource, corresponds to EMPI in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_con_PMRN}{string, Epic medical record number. This value is unique across Epic instances within the Partners network
#'  from \emph{con}datasource, corresponds to EPIC_PMRN in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_con_loc}{string, if mrn_type == TRUE, then the data in \emph{MRN_Type} and \emph{MRN} are parsed into IDs corresponding to locations \emph{(loc)}. Data is formatted using pretty_mrn().}
#'  \item{ID_con_loc_list}{string, if prevalence of IDs in \emph{Patient_ID_List} > \emph{perc}, then they are included in the output. Data is formatted using pretty_mrn().}
#'  \item{name_last}{string, Patient's last name, corresponds to Last_Name in RPDR.}
#'  \item{name_first}{string, Patient's first name, corresponds to First_Name in RPDR.}
#'  \item{name_middle}{string, Patient's middle name or initial, corresponds to Middle_Name in RPDR.}
#'  \item{name_previous}{string, Any alternate names on record for this patient, corresponds to Previous_Name in RPDR.}
#'  \item{SSN}{string, Social Security Number, corresponds to SSN in RPDR.}
#'  \item{VIP}{character, Special patient statuses as defined by the EMPI group, corresponds to VIP in RPDR.}
#'  \item{address1}{string, Patient's current address, corresponds to address1 in RPDR.}
#'  \item{address2}{string, Additional address information, corresponds to address2 in RPDR.}
#'  \item{city}{string, City of residence, corresponds to City in RPDR.}
#'  \item{state}{string, State of residence, corresponds to State in RPDR.}
#'  \item{country_con}{string, Country of residence from con datasource, corresponds to Country in RPDR.}
#'  \item{zip_con}{numeric, Mailing zip code of primary residence from con datasource, corresponds to Zip in RPDR. Formatted to 5 character zip codes using \emph{pretty_numbers()}.}
#'  \item{direct_contact_consent}{boolean, Indicates whether the patient has given permission to contact them directly through the RODY program, corresponds to Direct_Contact_Consent in RPDR. Legacy variable.}
#'  \item{research_invitations}{boolean, Indicates if a patient can be invited to participate in research, corresponds to Research_Invitations in RPDR.}
#'  \item{phone_home}{number, Patient's home phone number, corresponds to Home_Phone in RPDR. Formatted to 10 digit phone numbers using \emph{pretty_numbers()}.}
#'  \item{phone_day}{number, Phone number where the patient can be reached during the day, corresponds to Day_Phone in RPDR. Formatted to 10 digit phone numbers using \emph{pretty_numbers()}.}
#'  \item{insurance1}{string, Patient's primary health insurance carrier and subscriber ID information, corresponds to Insurance_1 in RPDR.}
#'  \item{insurance2}{string, Patient's secondary health insurance carrier and subscriber ID information, if any, corresponds to Insurance_2 in RPDR.}
#'  \item{insurance3}{string, Patient's tertiary health insurance carrier and subscriber ID information, if any, corresponds to Insurance_3 in RPDR.}
#'  \item{primary_care_physician}{string, Comma-delimited list of all primary care providers on record for this patient per institution, along with contact information (if available),
#'  corresponds to Primary_Care_Physician in RPDR.}
#'  \item{primary_care_physician_resident}{string, Comma-delimited list of any Resident primary care providers on record for this patient per institution, along with contact information (if available),
#'  corresponds to Resident _Primary_Care_Physician in RPDR.}
#'  }
#'
#' @encoding UTF-8
#'
#' @examples \dontrun{
#' #Using defaults
#' d_con <- load_con(file = "test_Con.txt")
#'
#' #Use sequential processing
#' d_con <- load_con(file = "test_Con.txt", nThread = 1)
#'
#' #Use parallel processing and parse data in
#' #MRN_Type and MRN columns (default in load_con) and keep all IDs
#' d_con <- load_con(file = "test_Con.txt", nThread = 20, mrn_type = TRUE, perc = 1)
#' }

load_con <- function(file, merge_id = "EMPI", sep = ":", id_length = "standard", perc = 0.6, na = TRUE, identical = TRUE, nThread = parallel::detectCores()-1, mrn_type = TRUE) {

  DATA <- load_base(file = file, merge_id = merge_id, sep = sep, id_length = id_length, perc = perc, na = na, identical = identical, nThread = nThread, mrn_type = mrn_type, src = "con")
  raw_id <- which(colnames(DATA) == "EMPI" | colnames(DATA) == "IncomingId")[1]
  data_raw <- DATA[, raw_id:dim(DATA)[2]]
  DATA     <- DATA[, 1:(raw_id-1)]

  #Parse Patient_ID_list
  message("Parsing Patient_ID_List column for all possible IDs present in at least ", perc*100, "% of the patients.")
  ids_l <- parse_ids(str = data_raw$Patient_ID_List, sep = sep, id_length = id_length, perc = perc, nThread = nThread)
  colnames(ids_l) <- paste0("ID_con_", colnames(ids_l), "_list")

  DATA <- cbind(DATA, ids_l)

  #Add additional information
  DATA$name_last      <- pretty_text(data_raw$Last_Name)
  DATA$name_first     <- pretty_text(data_raw$First_Name)
  DATA$name_middle    <- pretty_text(data_raw$Middle_Name)
  DATA$name_previous  <- pretty_text(data_raw$Previous_Name)
  DATA$SSN          <- pretty_text(data_raw$SSN)
  DATA$VIP          <- pretty_text(data_raw$VIP)
  DATA$address1     <- pretty_text(data_raw$Address1)
  DATA$address2     <- pretty_text(data_raw$Address2)
  DATA$city         <- pretty_text(data_raw$City)
  DATA$state        <- pretty_text(data_raw$State)
  DATA$country_con  <- pretty_text(data_raw$Country)
  DATA$zip_con      <- pretty_numbers(data_raw$Zip)
  DATA$direct_contact_consent <- pretty_text(data_raw$Direct_Contact_Consent)
  DATA$research_invitations   <- pretty_text(data_raw$Research_Invitations)
  DATA$phone_home <- pretty_numbers(data_raw$Home_Phone, length_final = 10, remove_from_back = NULL)
  DATA$phone_day  <- pretty_numbers(data_raw$Day_Phone, length_final = 10, remove_from_back = NULL)
  DATA$insurance1 <- pretty_text(data_raw$Insurance_1)
  DATA$insurance2 <- pretty_text(data_raw$Insurance_2)
  DATA$insurance3 <- pretty_text(data_raw$Insurance_3)
  DATA$primary_care_physician <- pretty_text(data_raw$Primary_Care_Physician)
  DATA$primary_care_physician_resident <- pretty_text(data_raw$Resident_Primary_Care_Physician)

  if(dim(DATA)[1] != 1) {DATA <- remove_column(dt = DATA, na = na, identical = identical)}
  return(DATA)
}

