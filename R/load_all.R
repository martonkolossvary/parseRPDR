#' @title Loads allergy data information into R.
#' @export
#'
#' @description Loads allergy information into the R environment.
#'
#' @param file string, full file path to All.txt.
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
#' @return data table, with allergy information.
#' \describe{
#'  \item{ID_MERGE}{numeric, defined IDs by \emph{merge_id}, used for merging later.}
#'  \item{ID_all_EMPI}{string, Unique Partners-wide identifier assigned to the patient used to consolidate patient information
#'  from \emph{all} datasource, corresponds to EMPI in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_all_PMRN}{string, Epic medical record number. This value is unique across Epic instances within the Partners network
#'  from \emph{all} datasource, corresponds to EPIC_PMRN in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_all_loc}{string, if mrn_type == TRUE, then the data in \emph{MRN_Type} and \emph{MRN} are parsed into IDs corresponding to locations \emph{(loc)}. Data is formatted using pretty_mrn().}
#'  \item{time_all}{POSIXct, Date when the allergy was first noted, corresponds to Noted_Date in RPDR. Converted to POSIXct format.}
#'  \item{all_all}{string, Name of the allergen, corresponds to Allergen in RPDR.}
#'  \item{all_all_code}{string, Epic internal identifier for the specific allergen, corresponds to Allergen_Code in RPDR.}
#'  \item{all_all_type}{string, Hierarchy for the type of allergy noted. Denotes known level of specificity of allergen, corresponds to Allergen_Type in RPDR.}
#'  \item{all_reac}{string, Noted reactions to the allergen, corresponds to Reactions in RPDR.}
#'  \item{all_reac_type}{string, Category of reaction to the allergen, corresponds to Reaction_Type in RPDR.}
#'  \item{all_severity}{string, Degree of severity of noted reactions, corresponds to Severity in RPDR.}
#'  \item{all_status}{string, Last known status of allergen, either active or deleted from the patient's allergy record, corresponds to Status in RPDR.}
#'  \item{all_system}{string, The source system where the data was collected, corresponds to System in RPDR.}
#'  \item{all_comment}{string, Free-text information about the allergen, corresponds to Comments in RPDR.}
#'  \item{all_del_reason}{string, Free-text information about why the allergen was removed from the patient's allergy list, corresponds to Deleted_Reason in RPDR.}
#'  }
#'
#' @encoding UTF-8
#'
#' @examples \dontrun{
#' #Using defaults
#' d_all <- load_all(file = "test_All.txt")
#'
#' #Use sequential processing
#' d_all <- load_all(file = "test_All.txt", nThread = 1)
#'
#' #Use parallel processing and parse data in MRN_Type and MRN columns and keep all IDs
#' d_all <- load_all(file = "test_All.txt", nThread = 20, mrn_type = TRUE, perc = 1)
#' }

load_all <- function(file, merge_id = "EMPI", sep = ":", id_length = "standard", perc = 0.6, na = TRUE, identical = TRUE, nThread = parallel::detectCores()-1, mrn_type = FALSE) {

  DATA <- load_base(file = file, merge_id = merge_id, sep = sep, id_length = id_length, perc = perc, na = na, identical = identical, nThread = nThread, mrn_type = mrn_type, src = "all")
  raw_id <- which(colnames(DATA) == "EMPI" | colnames(DATA) == "IncomingId")[1]
  data_raw <- DATA[, raw_id:dim(DATA)[2]]
  DATA     <- DATA[, 1:(raw_id-1)]

  #Add additional information
  DATA$time_all <- as.POSIXct(data_raw$Noted_Date, format = "%m/%d/%Y")
  DATA$all_all  <- pretty_text(data_raw$Allergen)
  DATA$all_all_code <- pretty_text(data_raw$Allergen_Code)
  DATA$all_all_type <- pretty_text(data_raw$Allergen_Type)
  DATA$all_reac      <- pretty_text(data_raw$Reactions)
  DATA$all_reac_type <- pretty_text(data_raw$Reaction_Type)
  DATA$all_severity  <- pretty_text(data_raw$Severity)
  DATA$all_status    <- pretty_text(data_raw$Status)
  DATA$all_system    <- pretty_text(data_raw$System)
  DATA$all_comment   <- pretty_text(data_raw$Comments)
  DATA$all_del_reason <- pretty_text(data_raw$Deleted_Reason_Comments)

  if(dim(DATA)[1] != 1) {DATA <- remove_column(dt = DATA, na = na, identical = identical)}
  return(DATA)
}
