#' @title Loads laboratory results into R.
#' @export
#'
#' @description Loads laboratory results into the R environment, both Lab and Clb files.
#'
#' @param file string, full file path to Lab.txt or Clb.txt.
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
#' @return data table, with laboratory exam information.
#' \describe{
#'  \item{ID_MERGE}{numeric, defined IDs by \emph{merge_id}, used for merging later.}
#'  \item{ID_lab_EMPI}{string, Unique Partners-wide identifier assigned to the patient used to consolidate patient information
#'  from \emph{lab} datasource, corresponds to EMPI in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_lab_PMRN}{string, Epic medical record number. This value is unique across Epic instances within the Partners network
#'  from \emph{lab} datasource, corresponds to EPIC_PMRN in RPDR. Data is formatted using pretty_mrn().}
#'  \item{ID_lab_loc}{string, if mrn_type == TRUE, then the data in \emph{MRN_Type} and \emph{MRN} are parsed into IDs corresponding to locations \emph{(loc)}. Data is formatted using pretty_mrn().}
#'  \item{time_lab_result}{POSIXct, Date when the specimen was collected, corresponds to Seq_Date_Time in RPDR. Converted to POSIXct format.}
#'  \item{lab_group}{string, Higher-level grouping concept used to consolidate similar tests across hospitals, corresponds to Group_ID in RPDR.}
#'  \item{lab_loinc}{string, Standardized LOINC code for the laboratory test, corresponds to Loinc_Code in RPDR.}
#'  \item{lab_testID}{string, Internal identifier for the test used by the source system, corresponds to Test_ID in RPDR.}
#'  \item{lab_descript}{string, Name of the lab test, corresponds to Test_Description in RPDR.}
#'  \item{lab_result}{string, Result value for the test, corresponds to Result in RPDR.}
#'  \item{lab_result_txt}{string, Additional information included with the result. This can include instructions for interpretation or comments from the laboratory, corresponds to Result_Text in RPDR.}
#'  \item{lab_result_abn}{string, Flag for identifying if values are outside of normal ranges or represent a significant deviation from previous values, corresponds to Abnormal_Flag in RPDR.}
#'  \item{lab_result_unit}{string, Units associated with the result value, corresponds to Reference_Unit in RPDR.}
#'  \item{lab_result_range}{string, Normal or therapeutic range for this value, corresponds to Reference_Range in RPDR.}
#'  \item{lab_result_toxic}{string, Reference range of values defined as being toxic to the patient, corresponds to Toxic_Range in RPDR.}
#'  \item{lab_spec}{string, Type of specimen collected to perform the test, corresponds to Specimen_Type in RPDR.}
#'  \item{lab_spec_txt}{string, Free-text information about the specimen, its collection or its integrity, corresponds to Specimen_Text in RPDR.}
#'  \item{lab_correction}{string, Free-text information about any changes made to the results, corresponds to Correction_Flag in RPDR.}
#'  \item{lab_status}{string, Flag which indicates whether the procedure is pending or complete, corresponds to Test_Status in RPDR.}
#'  \item{lab_ord_pys}{string, Name of the ordering physician, corresponds to Ordering_Doc in RPDR.}
#'  \item{lab_accession}{string, Internal tracking number assigned to the specimen for identification in the lab, corresponds to Accession in RPDR.}
#'  \item{lab_source}{string, Database source, either CDR (Clinical Data Repository) or RPDR (internal RPDR database), corresponds to Source in RPDR.}
#'  }
#'
#' @encoding UTF-8
#'
#' @examples \dontrun{
#' #Using defaults
#' d_lab <- load_lab(file = "test_Lab.txt")
#'
#' #Use sequential processing
#' d_lab <- load_lab(file = "test_Lab.txt", nThread = 1)
#'
#' #Use parallel processing and parse data in MRN_Type and MRN columns and keep all IDs
#' d_clb <- load_lab(file = "test_Clb.txt", nThread = 20, mrn_type = TRUE, perc = 1)
#' }

load_lab <- function(file, merge_id = "EMPI", sep = ":", id_length = "standard", perc = 0.6, na = TRUE, identical = TRUE, nThread = parallel::detectCores()-1, mrn_type = FALSE) {

  DATA <- load_base(file = file, merge_id = merge_id, sep = sep, id_length = id_length, perc = perc, na = na, identical = identical, nThread = nThread, mrn_type = mrn_type, src = "lab")
  raw_id <- which(colnames(DATA) == "EMPI" | colnames(DATA) == "IncomingId")[1]
  data_raw <- DATA[, raw_id:dim(DATA)[2]]
  DATA     <- DATA[, 1:(raw_id-1)]

  #Add additional information
  DATA$time_lab         <- as.POSIXct(data_raw$Seq_Date_Time, format = "%m/%d/%Y %H:%M")
  DATA$lab_group        <- pretty_text(data_raw$Group_Id)
  DATA$lab_loinc        <- pretty_text(data_raw$Loinc_Code)
  DATA$lab_testID       <- pretty_text(data_raw$Test_Id)
  DATA$lab_descript     <- pretty_text(data_raw$Test_Description)
  DATA$lab_result       <- pretty_text(data_raw$Result)
  DATA$lab_result_txt   <- pretty_text(data_raw$Result_Text)
  DATA$lab_result_abn   <- pretty_text(data_raw$Abnormal_Flag)
  DATA$lab_result_unit  <- pretty_text(data_raw$Reference_Units)
  DATA$lab_result_range <- pretty_text(data_raw$Reference_Range)
  DATA$lab_result_toxic <- pretty_text(data_raw$Toxic_Range)
  DATA$lab_spec         <- pretty_text(data_raw$Specimen_Type)
  DATA$lab_spec_txt     <- pretty_text(data_raw$Specimen_Text)
  DATA$lab_correction   <- pretty_text(data_raw$Correction_Flag)
  DATA$lab_status       <- pretty_text(data_raw$Test_Status)
  DATA$lab_ord_pys      <- pretty_text(data_raw$Ordering_Doc_Name)
  DATA$lab_source       <- pretty_text(data_raw$Source)
  DATA$lab_accession    <- pretty_text(data_raw$Accession)

  if(dim(DATA)[1] != 1) {DATA <- remove_column(dt = DATA, na = na, identical = identical)}
  return(DATA)
}
