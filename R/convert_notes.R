#' @title Extracts information from notes free text.
#' @export
#'
#' @description Analyzes notes loaded using \emph{load_notes} or \emph{load_lno}. Extracts information from the free text present in \emph{abc_rep_txt},
#' where \emph{abc} stands for the three letter abbreviation of the given type of note.
#' An array of string is provided using the \emph{anchors} argument. The function will return as many columns as there are anchor points.
#' Each column will contain the text between the given anchor point and the next following anchor point.
#' This way the free text report is split into corresponding smaller texts. By default, these are the common standard elements of given note types.
#' Here are provided potential anchor points for the given types of notes:
#'
#' \describe{
#' \item{Cardiology: }{c("Report Number:", "Report Status:", "Type:", "Date:", "Ordering Provider:", "SYSTOLIC BLOOD PRESSURE",  "DIASTOLIC BLOOD PRESSURE", "VENTRICULAR RATE EKG/MIN", "ATRIAL RATE", "PR INTERVAL", "QRS DURATION", "QT INTERVAL", "QTC INTERVAL",  "P AXIS", "R AXIS", "T WAVE AXIS", "LOC", "DX:", "REF:", "Electronically Signed", "report_end")}
#' \item{Discharge: }{c("***This text report", "Patient Information", "Physician Discharge Summary", "Surgeries this Admission", "Items for Post-Hospitalization Follow-Up:", "Pending Results", "Hospital Course", "ED Course:", "Diagnosis", "Prescriptions prior to admission", "Family History:", "Physical Exam on Admission:", "Discharge Exam", "report_end")}
#' \item{Endoscopy: }{c("NAME:", "DATE:", "Patient Information", "report_end")}
#' \item{History & Physical: }{c("***This text report", "Patient Information", "H&P by", "Author:", "Service:", "Author Type:", "Filed:", "Note Time:", "Status:", "Editor:", "report_end")}
#' \item{Operative: }{c("NAME:", "UNIT NO:, "DATE:", "SURGEON:", "ASST:", "PREOPERATIVE DIAGNOSIS:", "POSTOPERATIVE DIAGNOSIS:", "NAME OF OPERATION:", "ANESTHESIA:", "INDICATIONS", "OPERATIVE FINDINGS:", "DESCRIPTION OF PROCEDURE:", "Electronically Signed", "report_end")}
#' \item{Pathology: }{c("Accession Number:", "Report Status:", "Type:", "Report:", "CASE:", "PATIENT:", "Date", "Source Care Unit:", "Path Subspecialty Service:", "Results To:", "Signed Out by:", "CLINICAL DATA:", "FINAL DIAGNOSIS:", "GROSS DESCRIPTION:", "report_end")}
#' \item{Progress: }{c("***This text report", "Patient Information", "History", "Overview", "Progress Notes", "Medications", "Relevant Orders", "Level of Service", "report_end")}
#' \item{Pulmonary: }{c("The Pulmonary document", "Name:", "Unit #:", "Date:", "Location:", "Smoking Status:", "Pack Years:", "SPIROMETRY:", "LUNG VOLUMES:", "DIFFUSION:", "PLETHYSMOGRAPHY:" "Pulmonary Function Test Interpretation", "Spirometry", "report_end")}
#' \item{Radiology: }{c("Exam Code", "Ordering Provider", "HISTORY", "Associated Reports", "Report Below", "REASON", "REPORT",  "TECHNIQUE", "COMPARISON", "FINDINGS", "IMPRESSION", "RECOMMENDATION", "SIGNATURES", "report_end")}
#' \item{Visit: }{c("***This text report", "Reason for Visit", "Reason for Visit", "Vital Signs", "Chief Complaint", "History", "Overview", "Medications", "Relevant Orders", "Level of Service", "report_end"}
#' \item{LMR: }{c("Subject", "Patient Name:", "Reason for visit", "report_end"}
#' }
#'
#' However, these may be modified and extended to include sections of interest, i.e. if a given score is reported in a standard fashion, then adding this phrase (i.e. "CAD-RADS")
#' would create a column where the text following this statement is returned. After this the resulting columns can be easily cleaned up if needed.
#' Be aware to always include \emph{"report_end"} in the anchors array, to provide the function of the last occurring statement in the report.
#'
#' @param d data.table, database containing notes loaded using the \emph{load_notes} function.
#' @param code string vector, column name containing the results, which should be \emph{"abc_rep_txt"}, where \emph{abc} stands for the three letter abbreviation of the given type of note.
#' @param anchors string array, elements to search for in the text report.
#' @param nThread integer, number of threads to use for parallelization. If it is set to 1, then no parallel backends are created and the function is executed sequentially.
#'
#' @return data.table, with new columns corresponding to elements in \emph{anchors}.
#'
#' @encoding UTF-8
#'
#' @examples \dontrun{
#' #Create columns with specific parts of the radiological report defined by anchors
#' data_rad_parsed <- convert_notes(d = data_rad, code = "rad_rep_txt",
#' anchors = c("Exam Code", "Ordering Provider", "HISTORY", "Associated Reports",
#' "Report Below", "REASON", "REPORT", "TECHNIQUE", "COMPARISON", "FINDINGS",
#' "IMPRESSION", "RECOMMENDATION", "SIGNATURES", "report_end"), nThread = 2)
#' }

convert_notes <- function(d, code = NULL, anchors = NULL, nThread = parallel::detectCores()-1) {
  .SD=.N=.I=.GRP=.BY=.EACHI=..=..cols=.SDcols=i=j=time_to_db=..which_ids_to=..which_ids_from=..collapse <- NULL
  options(future.globals.maxSize = +Inf)

  message(paste0("Extracting information from notes free text."))

  #Initialize multicore
  if(nThread == 1) {
    `%exec%` <- foreach::`%do%`
    future::plan(future::sequential)
  } else {
    if(parallelly::supportsMulticore()) {
      future::plan(future::multicore, workers = nThread)
    } else {
      future::plan(future::multisession, workers = nThread)
    }
    `%exec%` <- doFuture::`%dofuture%`
  }

  #Find text locations
  d_res <- d[[code]]
  l_loc <- lapply(anchors, function(x) {regexpr(d_res, pattern = x, ignore.case = FALSE, fixed = TRUE)})
  names(l_loc) <- anchors
  d_loc     <- as.data.frame(lapply(l_loc, as.numeric)); d_loc[d_loc == -1] <- NA
  d_length  <- as.data.frame(lapply(l_loc, attr, "match.lengt")); d_length[d_length == -1] <- NA


  #Run parallel on elements of anchors
  result <- foreach::foreach(i = 1:length(anchors), .combine="cbind",
                             .inorder=TRUE, .options.future = list(chunk.size = 1.0),
                             .errorhandling = c("pass"), .verbose=FALSE) %exec%
    {
      from    <- d_loc[[i]] + d_length[[i]] #nchar ID from to use text
      d_dist  <- d_loc - from #find closest anchors
      d_dist[d_dist < 0] <- NA #delete negative instances
      to_id   <- as.numeric(apply(X = d_dist, MARGIN = 1, FUN = which.min)) #which column contains smallest value
      to      <- d_dist[cbind(seq_along(to_id), to_id)] + from #get distance value from given column

      if(length(to) != 0) {
        #Get text
        txt <- substring(d_res, first = from, last  = to-1)
        txt <- trimws(txt, whitespace = "[[:punct:]]") #remove leading and trailing punctuation marks
        txt <- gsub("[[:blank:]]+", " ", txt) #Remove multiple blank spaces
        txt <- trimws(txt) #Remove leading and trailing white
      } else {
        txt <- NULL
      }
      txt
    }

  #Rename columns
  cols <- anchors[-length(anchors)]
  cols <- tolower(cols)
  cols <- gsub("[[:blank:]]", "_", cols)
  cols <- paste0(strsplit(code, "_")[[1]][1], "_rep_", cols)
  colnames(result) <- cols

  result <- cbind(d, result)

  on.exit(options(future.globals.maxSize = 1.0 * 1e9))
  future::plan(future::sequential)
  return(result)
}
