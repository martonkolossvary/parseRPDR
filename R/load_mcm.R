#' @title Loads match control data into R.
#' @export
#'
#' @description Loads match control tables into the R environment.
#'
#' @param file string, full file path to Mcm.txt.
#' @param sep string, divider between hospital ID and MRN. Defaults to \emph{:}.
#' @param id_length string, indicating whether to modify MRN length based-on required values \emph{id_length = standard}, or to keep lengths as is \emph{id_length = asis}.
#' @param na boolean, whether to remove columns with only NA values. Defaults to \emph{TRUE}.
#' @param identical boolean, whether to remove columns with identical values. Defaults to \emph{TRUE}.
#' @param nThread integer, number of threads to use to load data.
#'
#' @return data table, with matching data.
#' \describe{
#'  \item{ID_case_PMRN}{string, Epic PMRN value for a patient in the index cohort, corresponds to Case_Patient_EPIC_PMRN in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_case_EMPI}{string, EMPI value for a patient in the index cohort, corresponds to Case_Patient_EMPI in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_control_PMRN}{string, Epic PMRN value for a patient matched to a case in the index cohort, corresponds to Control_Patient_EPIC_PMRN in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_control_EMPI}{string, EMPI value for a control patient matched to a case in the index cohort, corresponds to Control_Patient_EMPI in RPDR. Data is formatted using pretty_mrn().}
#'  \item{match_strength}{string, Number of similar data points between the index patient and the control patient. This number corresponds to the number of controls (Age, Gender, etc.) chosen during the match control query creation process, corresponds to Match_Strength in RPDR.}
#'  }
#'
#' @encoding UTF-8
#'
#' @examples \dontrun{
#' #Using defaults
#' d_mcm <- load_mcm(file = "test_Mcm.txt")
#'
#' #Use sequential processing
#' d_mcm <- load_mcm(file = "test_Mcm.txt", nThread = 1)
#'
#' #Use parallel processing and parse data in MRN_Type and MRN columns and keep all IDs
#' d_mcm <- load_mcm(file = "test_Mcm.txt", nThread = 20, mrn_type = TRUE, perc = 1)
#' }

load_mcm <- function(file, sep = ":", id_length = "standard", na = TRUE, identical = TRUE, nThread = parallel::detectCores()-1) {

  data_raw <- data.table::fread(file = file, nThread = nThread, colClasses = "character")
  DATA <- data.table::data.table()

  #Add additional information
  DATA$ID_case_PMRN <- pretty_mrn(v = data_raw$Case_Patient_EPIC_PMRN, prefix = "PMRN", sep = sep, id_length = id_length, nThread = nThread)
  DATA$ID_case_EMPI <- pretty_mrn(v = data_raw$Case_Patient_EMPI, prefix = "EMPI", sep = sep, id_length = id_length, nThread = nThread)
  DATA$ID_control_PMRN <- pretty_mrn(v = data_raw$Control_Patient_EPIC_PMRN, prefix = "PMRN", sep = sep, id_length = id_length, nThread = nThread)
  DATA$ID_control_EMPI <- pretty_mrn(v = data_raw$Control_Patient_EMPI, prefix = "EMPI", sep = sep, id_length = id_length, nThread = nThread)
  DATA$match_strength <- pretty_text(data_raw$Match_Strength)

  if(dim(DATA)[1] != 1) {DATA <- remove_column(dt = DATA, na = na, identical = identical)}
  return(DATA)
}
