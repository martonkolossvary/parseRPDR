#' @title Helper function for loading RPDR data into R.
#' @keywords internal
#'
#' @description Helper function to load different datasources from RPDR. Should not be used on its own.
#'
#' @param file string, full file path to given RPDR txt.
#' @param merge_id string, column name to use to create \emph{ID_MERGE} column used to merge different datasets. Defaults to \emph{EMPI},
#' as it is the preferred MRN in the RPDR system. In case of mrn dataset, leave at EMPI, as it is automatically converted to: "Enterprise_Master_Patient_Index".
#' @param sep string, divider between hospital ID and MRN. Defaults to \emph{:}.
#' @param id_length string, indicating whether to modify MRN length based-on required values \emph{id_length = standard}, or to keep lengths as is \emph{id_length = asis}.
#' If \emph{id_length = standard} then in case of \emph{MGH, BWH, MCL, EMPI and PMRN} the length of the MRNs are corrected accordingly by adding zeros, or removing numeral from the beginning.
#' In other cases the lengths are unchanged. Defaults to \emph{standard}.
#' @param perc numeric, a number between 0-1 indicating which parsed ID columns to keep. Data present in \emph{perc x 100\%} of patients are kept.
#' @param na boolean, whether to remove columns with only NA values. Defaults to \emph{TRUE}.
#' @param identical boolean, whether to remove columns with identical values. Defaults to \emph{TRUE}.
#' @param nThread integer, number of threads to use to load data.
#' @param mrn_type boolean, should data in \emph{MRN_Type} and \emph{MRN} be parsed. Defaults to \emph{FALSE}, as it is not advised to parse these for all data sources as it takes considerable time.
#' @param src string, what is the three letter source ID of the file, such as  \emph{dem}.
#'
#' @return data table, with minimally parsed data and the raw data.
#' \describe{
#'  \item{ID_MERGE}{numeric, defined IDs by \emph{merge_id}, used for merging later.}
#'  \item{ID_src_EMPI}{string, EMPI IDs from \emph{src} datasource, if the datasource is not mrn. Data is formatted using pretty_mrn().}
#'  \item{ID_src_PMRN}{string, PMRN IDs from \emph{src} datasource, if the datasource is not mrn. Data is formatted using pretty_mrn().}
#'  \item{ID_scr_loc}{string, from datasource \emph{src}, if mrn_type == TRUE, then the data in \emph{MRN_Type} and \emph{MRN} are parsed into IDs corresponding to locations \emph{(loc)}. Data is formatted using pretty_mrn().}
#'  }
#'
#' @encoding UTF-8
#' @importFrom data.table :=

load_base <- function(file, merge_id = "EMPI", sep = ":", id_length = "standard", perc = 0.6, na = TRUE, identical = TRUE, nThread = parallel::detectCores()-1, mrn_type = FALSE, src = "mrn", fill = FALSE, sep_load = "|") {
  message("Loading ", src,  " information into the R environment. Could take considerable time, please be patient!")
  data_raw <- data.table::fread(file, showProgress = FALSE, colClasses = "character", nThread = nThread, fill = fill, sep = sep_load)
  DATA <- data.table::data.table()
  if(src == "mrn" & merge_id == "EMPI") {
    DATA$ID_MERGE <- data_raw[["Enterprise_Master_Patient_Index"]]
  } else {
    DATA$ID_MERGE <- data_raw[[merge_id]]
  }

  if(src != "mrn") {
    EMPI_str <- paste0("ID_", src, "_EMPI")
    PMRN_str <- paste0("ID_", src, "_PMRN")

    DATA[, (EMPI_str)] <- pretty_mrn(v = data_raw$EMPI, prefix = "EMPI", sep = sep, id_length = id_length, nThread = nThread)
    DATA[, (PMRN_str)] <- pretty_mrn(v = data_raw$EPIC_PMRN, prefix = "PMRN", sep = sep, id_length = id_length, nThread = nThread)

    if(mrn_type) {
      #Parse MRN_Type to get IDs
      message(paste0("Parsing MRN_Type and MRN columns for all possible IDs present in at least ", perc*100, "% of the patients."))
      ids <- parse_ids(str = data_raw$MRN_Type, num = data_raw$MRN, sep = sep, id_length = id_length, perc = perc, nThread = nThread)
      colnames(ids) <- paste0("ID_", src, "_", colnames(ids))
      suppressWarnings(ids[, (EMPI_str):=NULL]); suppressWarnings(ids[, (PMRN_str):=NULL])

      DATA <- cbind(DATA, ids)
    }
  }
  DATA <- cbind(DATA, data_raw)
  return(DATA)
}
