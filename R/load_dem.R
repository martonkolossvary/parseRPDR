#' @title Loads demographic information into R for new demographic tables following changes in the beginning of 2022.
#' @export
#'
#' @description Loads patient demographic  and vital status information into the R environment. Since version 0.2.2 of the software this function supports the new demographics table data definitions.
#'
#' @param file string, full file path to Dem.txt.
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
#' @return data table, with demographic information data.
#' \describe{
#'  \item{ID_MERGE}{numeric, defined IDs by \emph{merge_id}, used for merging later.}
#'  \item{ID_dem_EMPI}{string, Unique Partners-wide identifier assigned to the patient used to consolidate patient information.
#'  from \emph{dem} datasource, corresponds to EMPI in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_dem_PMRN}{string, Epic medical record number. This value is unique across Epic instances within the Partners network.
#'  from \emph{dem} datasource, corresponds to EPIC_PMRN in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_dem_loc}{string, if mrn_type == TRUE, then the data in \emph{MRN_Type} and \emph{MRN} are parsed into IDs corresponding to locations \emph{(loc)}. Data is formatted using pretty_mrn().}
#'  \item{gender_legal_sex}{string, Patient's legal sex, corresponds to Gender_Legal_Sex in RPDR.}
#'  \item{sex_at_birth}{string, Patient’s sex at time of birth, corresponds to Sex_at_Birth in RPDR.}
#'  \item{gender_identity}{string, Patient's personal conception of their gender, corresponds to Gender_Identity in RPDR.}
#'  \item{time_date_of_birth}{POSIXct, Patient's date of birth, corresponds to Date_of_Birth. Converted to POSIXct format.}
#'  \item{age}{string, Patient's current age (or age at death), corresponds to Age in RPDR.}
#'  \item{language}{string, Patient's preferred spoken language, corresponds to Language in RPDR.}
#'  \item{language_group}{string, Patient's preferred language: English or Non-English, corresponds to Language_Group in RPDR.}
#'  \item{race_1}{string, Patient's primary race, corresponds to Race1 in RPDR.}
#'  \item{race_2}{string, Patient's primary race if more than one race, corresponds to Race2 in RPDR.}
#'  \item{race_group}{string, Patient's Race Group as determined by Race1 and Race2, corresponds to Race_Group in RPDR.}
#'  \item{ethnic_group}{string, Patient's Ethnicity: Hispanic or Non Hispanic, corresponds to Ethnic_Group in RPDR.}
#'  \item{marital}{string, Patient's current marital status, corresponds to Marital_Status in RPDR.}
#'  \item{religion}{string, Patient-identified religious preference, corresponds to Religion in RPDR.}
#'  \item{veteran}{string, Patient's current military veteran status, corresponds to Is_a_veteran in RPDR.}
#'  \item{country_dem}{string, Patient's current country of residence from dem datasource, corresponds to Country in RPDR.}
#'  \item{zip_dem}{string, Mailing zip code of patient's primary residence from dem datasource, corresponds to Zip_code in RPDR.Formatted to 5 character zip codes.}
#'  \item{vital_status}{string, Identifies if the patient is living or deceased.
#'  This data is updated monthly from the Partners registration system and the Social Security Death Master Index, corresponds to Vital_Status in RPDR. Punctuation marks are removed.}
#'  \item{time_date_of_death}{POSIXct, Recorded date of death from source in 'Vital_Status'.
#'  Date of death information obtained solely from the Social Security Death Index will not be reported until 3 years after death due to privacy concerns.
#'  If the value is independently documented by a Partners entity within the 3 year window then the date will be displayed. corresponds to Date_of_Death in RPDR. Converted to POSIXct format.}
#'}
#'
#' @encoding UTF-8
#'
#' @examples \dontrun{
#' #Using defaults
#' d_dem <- load_dem(file = "test_Dem.txt")
#'
#' #Use sequential processing
#' d_dem <- load_dem(file = "test_Dem.txt", nThread = 1)
#'
#' #Use parallel processing and parse data in MRN_Type and MRN columns and keep all IDs
#' d_dem <- load_dem(file = "test_Dem.txt", nThread = 20, mrn_type = TRUE, perc = 1)
#' }

load_dem <- function(file, merge_id = "EMPI", sep = ":", id_length = "standard", perc = 0.6, na = TRUE, identical = TRUE, nThread = parallel::detectCores()-1, mrn_type = FALSE) {

  DATA <- load_base(file = file, merge_id = merge_id, sep = sep, id_length = id_length, perc = perc, na = na, identical = identical, nThread = nThread, mrn_type = mrn_type, src = "dem")
  raw_id <- which(colnames(DATA) == "EMPI" | colnames(DATA) == "IncomingId")[1]
  data_raw <- DATA[, raw_id:dim(DATA)[2]]
  DATA     <- DATA[, 1:(raw_id-1)]

  #Add additional information
  DATA$gender_legal_sex   <- pretty_text(data_raw$Gender_Legal_Sex)
  DATA$sex_at_birth       <- pretty_text(data_raw$Sex_At_Birth)
  DATA$gender_identity    <- pretty_text(data_raw$Gender_Identity)
  DATA$time_date_of_birth <- as.POSIXct(data_raw$Date_of_Birth, format = "%m/%d/%Y")
  DATA$age                <- pretty_text(data_raw$Age)
  DATA$language       <- pretty_text(data_raw$Language)
  DATA$language_group <- pretty_text(data_raw$Language_group)
  DATA$race_1     <- pretty_text(data_raw$Race1)
  DATA$race_2     <- pretty_text(data_raw$Race2)
  DATA$race_group <- pretty_text(data_raw$Race_Group)
  DATA$ethnic_group <- pretty_text(data_raw$Ethnic_Group)
  DATA$marital  <- pretty_text(data_raw$Marital_status)
  DATA$religion <- pretty_text(data_raw$Religion)
  DATA$veteran  <- pretty_text(data_raw$Is_a_veteran)
  DATA$country_dem  <- pretty_text(data_raw$Country)
  DATA$zip_dem      <- pretty_numbers(data_raw$Zip_code)
  DATA$vital_status       <- pretty_text(data_raw$Vital_status)
  DATA$time_date_of_death <- as.POSIXct(data_raw$Date_Of_Death, format = "%m/%d/%Y")

  if(dim(DATA)[1] != 1) {DATA <- remove_column(dt = DATA, na = na, identical = identical)}
  return(DATA)
}
