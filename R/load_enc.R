#' @title Loads encounter information into R.
#' @export
#'
#' @description Loads encounter-level detail information into the R environment, both Enc and Exc files.
#'
#' @param file string, full file path to Enc.txt or Exc.txt
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
#' @return data table, with encounter information.
#' \describe{
#'  \item{ID_MERGE}{numeric, defined IDs by \emph{merge_id}, used for merging later.}
#'  \item{ID_enc_EMPI}{string, Unique Partners-wide identifier assigned to the patient used to consolidate patient information
#'  from \emph{enc} datasource, corresponds to EMPI in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_enc_PMRN}{string, Epic medical record number. This value is unique across Epic instances within the Partners network
#'  from \emph{enc} datasource, corresponds to EPIC_PMRN in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_enc_loc}{string, if mrn_type == TRUE, then the data in \emph{MRN_Type} and \emph{MRN} are parsed into IDs corresponding to locations \emph{(loc)}. Data is formatted using pretty_mrn().}
#'  \item{enc_numb}{string, Unique identifier of the record/visit. This values includes the source system, hospital, and a unique identifier within the source system, corresponds to Encounter_number in RPDR.}
#'  \item{time_enc_admit}{POSIXct, Date when the patient was admitted or entered the facility, corresponds to Admit_Date in RPDR. Converted to POSIXct format.}
#'  \item{time_enc_disch}{POSIXct, Date when the patient was discharged or left the facility, corresponds to Discharge_Date in RPDR. Converted to POSIXct format.}
#'  \item{enc_status}{string, Billing account-related notes about the encounter. This will not be populated for all encounters, corresponds to Encounter_Status in RPDR.}
#'  \item{enc_hosp}{string, Facility where the encounter occurred, corresponds to Hospital in RPDR.}
#'  \item{enc_inpatient}{string, Classifies the type of encounter as either Inpatient or Outpatient. ED visits are currently classified under the 'Outpatient' label, corresponds to Inpatient_or_Outpatient in RPDR.}
#'  \item{enc_service}{string, Hospital service line assigned to the encounter, corresponds to Service_Line in RPDR.}
#'  \item{enc_attending}{string, The attending provider associated with the encounter. For Epic professional billing, this is the billing provider, corresponds to Attending_MD in RPDR.}
#'  \item{enc_length}{numeric, Length of stay for the encounter, corresponds to LOS_days in RPDR.}
#'  \item{enc_clinic}{string, Specific department/location where the encounter occured, corresponds to Clinic_Name in RPDR.}
#'  \item{enc_admit_src}{string, Location where the patient was admitted when entering the hospital/clinic, corresponds to Admit_Source in RPDR.}
#'  \item{enc_pat_type}{string, Provides information regarding the specific patient classifications and status of the patient visit. This field is only populated for McLean Hospital encounters, corresponds to Patient_Type in RPDR.}
#'  \item{enc_ref_disp}{string, Location where the patient has been directed for treatment or follow-up by a staff member. This field is only populated for McLean Hospital encounters, corresponds to Referrer_Discipline in RPDR.}
#'  \item{enc_disch_disp}{string, Patient's anticipated location or status following the encounter, corresponds to Discharge_Disposition in RPDR.}
#'  \item{enc_pay}{string, Payors responsible for the hospital account. Multiple payors (primary, secondary, etc.) may be listed, corresponds to Payor in RPDR.}
#'  \item{enc_diag_admit}{string, Initial working diagnosis documented by the admitting or attending physician, corresponds to Admitting_Diagnosis in RPDR.}
#'  \item{enc_diag_princ}{string, Condition established, after study, to be chiefly responsible for occasioning the admission of the patient to the hospital for care, corresponds to Principle_Diagnosis in RPDR.}
#'  \item{enc_diag_1}{string, Additional diagnoses associated with this encounter or visit, corresponds to Diagnosis_1 in RPDR.}
#'  \item{enc_diag_2}{string, Additional diagnoses associated with this encounter or visit, corresponds to Diagnosis_2 in RPDR.}
#'  \item{enc_diag_3}{string, Additional diagnoses associated with this encounter or visit, corresponds to Diagnosis_3 in RPDR.}
#'  \item{enc_diag_4}{string, Additional diagnoses associated with this encounter or visit, corresponds to Diagnosis_4 in RPDR.}
#'  \item{enc_diag_5}{string, Additional diagnoses associated with this encounter or visit, corresponds to Diagnosis_5 in RPDR.}
#'  \item{enc_diag_6}{string, Additional diagnoses associated with this encounter or visit, corresponds to Diagnosis_6 in RPDR.}
#'  \item{enc_diag_7}{string, Additional diagnoses associated with this encounter or visit, corresponds to Diagnosis_7 in RPDR.}
#'  \item{enc_diag_8}{string, Additional diagnoses associated with this encounter or visit, corresponds to Diagnosis_8 in RPDR.}
#'  \item{enc_diag_9}{string, Additional diagnoses associated with this encounter or visit, corresponds to Diagnosis_9 in RPDR.}
#'  \item{enc_diag_10}{string, Additional diagnoses associated with this encounter or visit, corresponds to Diagnosis_10 in RPDR.}
#'  \item{enc_diag_group}{string, Diagnosis-Related Group for the encounter, in the following format: SYSTEM:CODE - Description, corresponds to DRG in RPDR.}
#'  }
#' @encoding UTF-8
#'
#' @examples \dontrun{
#' #Using defaults
#' d_enc <- load_enc(file = "test_Enc.txt")
#'
#' #Use sequential processing
#' d_enc <- load_enc(file = "test_Enc.txt", nThread = 1)
#'
#' #Use parallel processing and parse data in MRN_Type and MRN columns and keep all IDs
#' d_exc <- load_enc(file = "test_Exc.txt", nThread = 20, mrn_type = TRUE, perc = 1)
#' }

load_enc <- function(file, merge_id = "EMPI", sep = ":", id_length = "standard", perc = 0.6, na = TRUE, identical = TRUE, nThread = parallel::detectCores()-1, mrn_type = FALSE) {

  DATA <- load_base(file = file, merge_id = merge_id, sep = sep, id_length = id_length, perc = perc, na = na, identical = identical, nThread = nThread, mrn_type = mrn_type, src = "enc")
  raw_id <- which(colnames(DATA) == "EMPI" | colnames(DATA) == "IncomingId")[1]
  data_raw <- DATA[, raw_id:dim(DATA)[2]]
  DATA     <- DATA[, 1:(raw_id-1)]

  #Add additional information
  DATA$time_enc_admit <- as.POSIXct(data_raw$Admit_Date, format = "%m/%d/%Y")
  DATA$time_enc_disch <- as.POSIXct(data_raw$Discharge_Date, format = "%m/%d/%Y")
  DATA$enc_status     <- pretty_text(data_raw$Encounter_Status)
  DATA$enc_clinic     <- pretty_text(data_raw$Clinic_Name)
  DATA$enc_hosp       <- pretty_text(data_raw$Hospital)
  DATA$enc_inpatient  <- pretty_text(data_raw$Inpatient_Outpatient)
  DATA$enc_service    <- pretty_text(data_raw$Service_Line)
  DATA$enc_attending  <- pretty_text(data_raw$Attending_MD)
  DATA$enc_length     <- pretty_text(data_raw$LOS_Days)
  DATA$enc_admit_src  <- pretty_text(data_raw$Admit_Source)
  DATA$enc_pat_type   <- pretty_text(data_raw$Patient_Type)
  DATA$enc_ref_disp   <- pretty_text(data_raw$Referrer_Discipline)
  DATA$enc_disch_disp <- pretty_text(data_raw$Discharge_Disposition)
  DATA$enc_pay        <- pretty_text(data_raw$Payor)

  DATA$enc_diag_admit <- pretty_text(data_raw$Admitting_Diagnosis)
  DATA$enc_diag_princ <- pretty_text(data_raw$Principal_Diagnosis)
  DATA$enc_diag_1     <- pretty_text(data_raw$Diagnosis_1)
  DATA$enc_diag_2     <- pretty_text(data_raw$Diagnosis_2)
  DATA$enc_diag_3     <- pretty_text(data_raw$Diagnosis_3)
  DATA$enc_diag_4     <- pretty_text(data_raw$Diagnosis_4)
  DATA$enc_diag_5     <- pretty_text(data_raw$Diagnosis_5)
  DATA$enc_diag_6     <- pretty_text(data_raw$Diagnosis_6)
  DATA$enc_diag_7     <- pretty_text(data_raw$Diagnosis_7)
  DATA$enc_diag_8     <- pretty_text(data_raw$Diagnosis_8)
  DATA$enc_diag_9     <- pretty_text(data_raw$Diagnosis_9)
  DATA$enc_diag_10    <- pretty_text(data_raw$Diagnosis_10)
  DATA$enc_diag_group <- pretty_text(data_raw$DRG)
  DATA$enc_enc_numb   <- pretty_text(data_raw$Encounter_number)

  if(dim(DATA)[1] != 1) {DATA <- remove_column(dt = DATA, na = na, identical = identical)}
  return(DATA)
}
