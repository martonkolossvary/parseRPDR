#' @title Loads transfusion results into R.
#' @export
#'
#' @description Loads transfusion results into the R environment.
#'
#' @param file string, full file path to Trn.txt
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
#' @return data table, with transfusion information.
#' \describe{
#'  \item{ID_MERGE}{numeric, defined IDs by \emph{merge_id}, used for merging later.}
#'  \item{ID_trn_EMPI}{string, Unique Partners-wide identifier assigned to the patient used to consolidate patient information
#'  from \emph{trn} datasource, corresponds to EMPI in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_trn_PMRN}{string, Epic medical record number. This value is unique across Epic instances within the Partners network
#'  from \emph{trn} datasource, corresponds to EPIC_PMRN in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_trn_loc}{string, if mrn_type == TRUE, then the data in \emph{MRN_Type} and \emph{MRN} are parsed into IDs corresponding to locations \emph{(loc)}. Data is formatted using pretty_mrn().}
#'  \item{time_trn}{POSIXct, Date when the transfusion was administered or test was performed, corresponds to Transaction_Date_Time in RPDR. Converted to POSIXct format.}
#'  \item{trn_descript}{string, The type of procedure or product administered, corresponds to Test_Description in RPDR.}
#'  \item{trn_result}{string, Results of the test or transaction/lot number of transfusion, corresponds to Results in RPDR.}
#'  \item{trn_result_abn}{string, Denotes an abnormal finding or value, corresponds to Abnormal_Flag in RPDR.}
#'  \item{trn_comment}{string, Free-text comments about the status of the test/transfusion, corresponds to Comments in RPDR.}
#'  \item{trn_status}{string, Completion status of the requested test/transfusion, corresponds to Status_Flag in RPDR.}
#'  \item{trn_accession}{string, Identifier assigned to the test/transfusion for tracking purposes by the blood bank, corresponds to Accession in RPDR.}
#'  }
#'
#' @encoding UTF-8
#'
#' @examples \dontrun{
#' #Using defaults
#' d_trn <- load_trn(file = "test_Trn.txt")
#'
#' #Use sequential processing
#' d_trn <- load_trn(file = "test_Trn.txt", nThread = 1)
#'
#' #Use parallel processing and parse data in MRN_Type and MRN columns and keep all IDs
#' d_trn <- load_trn(file = "test_Trn.txt", nThread = 20, mrn_type = TRUE, perc = 1)
#' }

load_trn <- function(file, merge_id = "EMPI", sep = ":", id_length = "standard", perc = 0.6, na = TRUE, identical = TRUE, nThread = parallel::detectCores()-1, mrn_type = FALSE) {

  DATA <- load_base(file = file, merge_id = merge_id, sep = sep, id_length = id_length, perc = perc, na = na, identical = identical, nThread = nThread, mrn_type = mrn_type, src = "trn")
  raw_id <- which(colnames(DATA) == "EMPI" | colnames(DATA) == "IncomingId")[1]
  data_raw <- DATA[, raw_id:dim(DATA)[2]]
  DATA     <- DATA[, 1:(raw_id-1)]

  #Add additional information
  DATA$time_trn         <- as.POSIXct(data_raw$Transaction_Date_Time, format = "%m/%d/%Y %H:%M")
  DATA$trn_descript     <- pretty_text(data_raw$Test_Description)
  DATA$trn_result       <- pretty_text(data_raw$Result)
  DATA$trn_result_abn   <- pretty_text(data_raw$Abnormal_Flag)
  DATA$trn_comment      <- pretty_text(data_raw$Comments)
  DATA$trn_status       <- pretty_text(data_raw$Status_Flag)
  DATA$trn_accession    <- pretty_text(data_raw$Accession)

  if(dim(DATA)[1] != 1) {DATA <- remove_column(dt = DATA, na = na, identical = identical)}
  return(DATA)
}
