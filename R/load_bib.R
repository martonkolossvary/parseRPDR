#' @title Loads BiobankFile data into R.
#' @export
#'
#' @description Loads Biobank file data into the R environment.
#'
#' @param file string, full file path to Bib.txt.
#' @param merge_id string, column name to use to create \emph{ID_MERGE} column used to merge different datasets. Defaults to \emph{EPIC_PMRN},
#' as it is the preferred MRN in the RPDR system.
#' @param sep string, divider between hospital ID and MRN. Defaults to \emph{:}.
#' @param id_length string, indicating whether to modify MRN length based-on required values \emph{id_length = standard}, or to keep lengths as is \emph{id_length = asis}.
#' If \emph{id_length = standard} then in case of \emph{MGH, BWH, MCL, EMPI and PMRN} the length of the MRNs are corrected accordingly by adding zeros, or removing numeral from the beginning.
#' In other cases the lengths are unchanged. Defaults to \emph{standard}.
#' @param perc numeric, a number between 0-1 indicating which parsed ID columns to keep. Data present in \emph{perc x 100\%} of patients are kept. Not used for loading mrn data.
#' @param na boolean, whether to remove columns with only NA values. Defaults to \emph{TRUE}.
#' @param identical boolean, whether to remove columns with identical values. Defaults to \emph{TRUE}.
#' @param nThread integer, number of threads to use to load data.
#' @param mrn_type boolean, should data in \emph{MRN_Type} and \emph{MRN} be parsed. Defaults to \emph{FALSE}, as it is not advised to parse these for all data sources as it takes considerable time.
#'
#' @return data table, with BiobankFile data.
#' \describe{
#'  \item{ID_MERGE}{numeric, defined IDs by \emph{merge_id}, used for merging later.}
#'  \item{ID_bib_PMRN}{string, Epic medical record number. This value is unique across Epic instances within the Partners network, corresponds to EPIC_PMRN in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_bib_EMPI}{string, Unique Partners-wide identifier assigned to the patient used to consolidate patient information, corresponds to Enterprise_Master_Patient_Index in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_bib_MGH}{string, Unique Medical Record Number for Mass General Hospital, corresponds to MGH_MRN in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_bib_BWH}{string, Unique Medical Record Number for Brigham and Women's Hospital, corresponds to BWH_MRN in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_bib_FH}{string, Unique Medical Record Number for Faulkner Hospital, corresponds to FH_MRN in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_bib_SRH}{string, Unique Medical Record Number for Spaulding Rehabilitation Hospital, corresponds to SRH_MRN in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_bib_NWH}{string, Unique Medical Record Number for Newton-Wellesley Hospital, corresponds to NWH_MRN in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_bib_NSMC}{string, Unique Medical Record Number for North Shore Medical Center, corresponds to NSMC_MRN in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_bib_MCL}{string, Unique Medical Record Number for McLean Hospital, corresponds to MCL_MRN in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_bib_MEE}{string, Unique Medical Record Number for Mass Eye and Ear, corresponds to MEE_MRN in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_bib_DFC}{string, Unique Medical Record Number for Dana Farber Cancer center, corresponds to DFC_MRN in RPDR. Data is formatted using pretty_mrn(). Legacy data.}
#'  \item{ID_bib_WDH}{string, Unique Medical Record Number for Wentworth-Douglass Hospital, corresponds to WDH_MRN in RPDR. Data is formatted using pretty_mrn(). Legacy data.}
#'  \item{bib_subject_ID}{string, Biobank unique patient identifier, corresponds to Subject_ID in RPDR. ID is not formatted.}
#'  \item{bib_subject_ID}{string, This will always default to Biobank, corresponds to Registry Name in RPDR.}
#'  }
#'
#' @encoding UTF-8
#'
#' @examples \dontrun{
#' #Using defaults
#' d_bib <- load_bib(file = "test_Bib.txt")
#'
#' #Use sequential processing
#' d_bib <- load_bib(file = "test_Bib.txt", nThread = 1)
#'
#' #Use parallel processing and parse data in MRN_Type and MRN columns and keep all IDs
#' d_bib <- load_bib(file = "test_Bib.txt", nThread = 20, mrn_type = TRUE, perc = 1)
#' }

load_bib <- function(file, merge_id = "EMPI", sep = ":", id_length = "standard", perc = 0.6, na = TRUE, identical = TRUE, nThread = parallel::detectCores()-1, mrn_type = FALSE) {

  DATA <- load_base(file = file, merge_id = merge_id, sep = sep, id_length = id_length, perc = perc, na = na, identical = identical, nThread = nThread, mrn_type = mrn_type, src = "bib")
  raw_id <- which(colnames(DATA) == "Registry_Name")[1]
  data_raw <- DATA[, raw_id:dim(DATA)[2]]
  DATA     <- DATA[, 1:(raw_id-1)]

  #Add additional information
  DATA$ID_bib_PMRN <- pretty_mrn(v = data_raw$EPIC_PMRN, prefix = "PMRN", sep = sep, id_length = id_length, nThread = nThread)
  DATA$ID_bib_EMPI <- pretty_mrn(v = data_raw$EMPI, prefix = "EMPI", sep = sep, id_length = id_length, nThread = nThread)
  DATA$ID_bib_MGH  <- pretty_mrn(v = data_raw$MGH_MRN, prefix = "MGH", sep = sep, id_length = id_length, nThread = nThread)
  DATA$ID_bib_BWH  <- pretty_mrn(v = data_raw$BWH_MRN, prefix = "BWH", sep = sep, id_length = id_length, nThread = nThread)
  DATA$ID_bib_FH   <- pretty_mrn(v = data_raw$FH_MRN, prefix = "FH", sep = sep, id_length = id_length, nThread = nThread)
  DATA$ID_bib_SRH  <- pretty_mrn(v = data_raw$SRH_MRN, prefix = "SRH", sep = sep, id_length = id_length, nThread = nThread)
  DATA$ID_bib_NWH  <- pretty_mrn(v = data_raw$NWH_MRN, prefix = "NWH", sep = sep, id_length = id_length, nThread = nThread)
  DATA$ID_bib_NSMC <- pretty_mrn(v = data_raw$NSMC_MRN, prefix = "NSMC", sep = sep, id_length = id_length, nThread = nThread)
  DATA$ID_bib_MCL  <- pretty_mrn(v = data_raw$MCL_MRN, prefix = "MCL", sep = sep, id_length = id_length, nThread = nThread)
  DATA$ID_bib_MEE  <- pretty_mrn(v = data_raw$MEE_MRN, prefix = "MEE", sep = sep, id_length = id_length, nThread = nThread)
  DATA$ID_bib_DFC  <- pretty_mrn(v = data_raw$DFC_MRN, prefix = "DFC", sep = sep, id_length = id_length, nThread = nThread)
  DATA$ID_bib_WDH  <- pretty_mrn(v = data_raw$WDH_MRN, prefix = "WDH", sep = sep, id_length = id_length, nThread = nThread)

  DATA$bib_subject_ID <- pretty_text(data_raw$Subject_Id)
  DATA$bib_reg_name   <- pretty_text(data_raw$Registry_Name)

  if(dim(DATA)[1] != 1) {DATA <- remove_column(dt = DATA, na = na, identical = identical)}
  return(DATA)
}
