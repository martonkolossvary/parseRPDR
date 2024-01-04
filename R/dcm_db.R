#' @title Internal function to create a database of DICOM headers.
#' @keywords internal
#'
#' @description The function creates a database of DICOM headers present in a folder structure. Each series should be in its own folder,
#' but they can be in a nested folder structure. Files where there are also folder present next to them at the same level will not be parsed.
#' That is the folder structure needs to comply with the DICOM standard.
#' Be aware that the function requires \href{https://www.python.org}{python} and \href{https://pydicom.github.io}{pydicom} to be installed!
#' The function cycles through all folders present in the provided path and recursively goes through them,
#' every subfolder, and extracts the DICOM header information from the files using the
#' \href{https://pydicom.github.io/pydicom/dev/reference/generated/pydicom.filereader.dcmread.html}{dcmread}
#' function of the \href{https://pydicom.github.io}{pydicom} package.
#' The extension of the files can be provided by the \emph{ext} argument,
#' as DICOM files may have different extensions then that of .dcm. Also, using the \emph{all} boolean argument, you can specify whether
#' the function provides output for each file, or only for the first file, which is beneficial if you are analyzing multi-slice series,
#' as all instances have almost all the same header information. Furthermore, using the \emph{keywords} argument you can manually specify
#' which DICOM keywords you wish to extract. These need to be a valid keyword specified in the
#' \href{https://dicom.nema.org/medical/dicom/current/output/chtml/part06/chapter_6.html}{DICOM standard}.
#'
#' @param path string vector, full folder path to folder that contains the images.
#' @param ext string array, possible file extensions to parse. It is advised to add \emph{.} before the extensions as the given character patterns
#' may be present elsewhere in the file names. Furthermore, if DICOM files without an extension should also be parsed, then add \emph{""} to the
#' extensions as then the script will try to read all files without an extension. Also, the file names and the extensions are converted to lower case
#' before matching to avoid mismatches due to capitals.
#' @param all  boolean, whether all files in a series should be parsed, or only the first one.
#' @param keywords string array, of valid DICOM keywords.
#' @param nThread integer, number of threads to use for parsing data.
#' @param pydicom package, pydicom package initiated from parent environment.
#'
#' @return data.table, with  DICOM header information. This is then used by \emph{create_img_db} which formats the output.
#'
#' @encoding UTF-8


dcm_db <- function(path, ext, all, keywords, nThread, pydicom) {

  # Format for regexp
  for(i in 1:length(ext)) {
    ext[i] = paste0(ext[i], "$") #Add from end character
    if(substr(ext[i], 1, 1) == ".") {ext[i] <- paste0("\\", ext[i])} #Add escape characters in case of "." at beginning
  }

  folders <- list.dirs(path, recursive=FALSE, full.names=FALSE)
  if (length(folders)==0) { #Find files
    file_short <- list.files(path, full.names = FALSE)
    file_used  <- list.files(path, full.names = TRUE)

    ok <- grep(paste0(tolower(ext), collapse = "|"), tolower(file_short))

    file_short <- file_short[ok]
    file_used  <- file_used[ok]

    if(!all) {file_used <- file_used[1]; file_short <- file_short[1]}
    if(all(is.na(file_used))) {
      out <- data.table::as.data.table(path)
      out
    } else {
      out <- NULL
      for(i in 1:length(file_used)){
        file      <- file_used[i]
        file_name <- file_short[i]
        dcm_header <- try(pydicom$dcmread(fp = file, stop_before_pixels = TRUE), silent = TRUE)
        if(inherits(dcm_header, "try-error")) {
          out <- rbind(out, cbind(data.table::as.data.table(path), data.table::as.data.table(file), data.table::as.data.table(file_name)))
        } else {
          dmc_info <- lapply(keywords, function(x) {
            value <- try(as.character(dcm_header[[x]]), silent = TRUE)
            if(inherits(value, "try-error")) {
              NA
            } else {
              value <- regmatches(value, regexpr("[^:]+(?=$)", value, perl=TRUE)) #From last :
              value <- gsub("[[:space:]]", "", value) #Remove whitespace
              value <- gsub("\\", "", value, fixed = TRUE) #Remove \
              value <- gsub('"', "", value, fixed = TRUE) #Remove "
              value <- gsub("'", "", value, fixed = TRUE) #Remove '
              value
            }})

          names(dmc_info) <- keywords
          dmc_info <- suppressWarnings(data.table::as.data.table(dmc_info))
          dmc_info <- cbind(data.table::as.data.table(path), data.table::as.data.table(file), data.table::as.data.table(file_name), dmc_info)
          out <- rbind(out, dmc_info)
        }
      }
      out
    }
  }
  else { #Go in sub-directory and repeat function recursively
    sublist   <- parallel::mclapply(paste0(path, "/", folders), function(x){
      dcm_db(path = x, ext = ext, all = all, keywords = keywords, nThread = nThread, pydicom = pydicom)
    }, mc.cores = nThread, mc.allow.recursive = TRUE)
    data.table::rbindlist(sublist, fill = TRUE) #create dt
  }
}
