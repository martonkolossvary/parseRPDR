#' @title Create a database of DICOM headers.
#' @export
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
#' @param na boolean, whether to remove columns with only NA values. Defaults to \emph{TRUE}.
#' @param identical boolean, whether to remove columns with identical values. Defaults to \emph{TRUE}.
#'
#' @return data.table, with DICOM header information return unchanged. However, the function also provides additional new columns
#' which help further data manipulations, these are:
#' \describe{
#'  \item{time_study}{POSIXct, StudyDate and StudyTime concatentated together to POSIXct.}
#'  \item{time_series}{POSIXct, SeriesDate and SeriesTime concatentated together to POSIXct.}
#'  \item{time_acquisition}{POSIXct, AcquisitionDate and AcquisitionTime concatentated together to POSIXct.}
#'  \item{name_img}{string, PatientName with special characters removed.}
#'  \item{time_date_of_birth_img}{POSIXct, PatientBirthDate as POSIXct.}
#'  \item{img_pixel_spacing}{numeric, PixelSpacing value of the first element in the array returned as numerical value.}
#'  }
#'
#' @encoding UTF-8
#'
#' @examples \dontrun{
#' #Create a database with DICOM header information
#' all_dicom_headers <- create_img_db(path = "/Users/Test/Data/DICOM/")
#  #Create a database with DICOM header information with additional file extensions
#' all_dicom_headers <- create_img_db(path = "/Users/Test/Data/DICOM/", ext = c(".dcm", ".DICOM"))
#' #Create a database with DICOM header information for only IDs and accession numbers
#' all_dicom_headers <- create_img_db(path = "/Users/Test/Data/DICOM/",
#' keywords = c("PatientID", "AccessionNumber"))
#' }

create_img_db <- function(path, ext = c(".dcm", ".dicom", ".ima", ".tmp", ""), all = TRUE, keywords = c("StudyDate", "StudyTime", "SeriesDate", "SeriesTime", "AcquisitionDate", "AcquisitionTime",
                                                                      "ConversionType", "Manufacturer", "InstitutionName", "InstitutionalDepartmentName",
                                                                      "ReferringPhysicianName", "Modality", "ManufacturerModelName",
                                                                      "StudyDescription", "SeriesDescription", "StudyComments", "ProtocolName", "RequestedProcedureID", "ViewPosition",
                                                                      "StudyInstanceUID", "SeriesInstanceUID", "SOPInstanceUID", "AccessionNumber",
                                                                      "PatientName", "PatientID", "IssuerOfPatientID", "PatientBirthDate", "PatientSex",
                                                                      "PatientAge", "PatientSize", "PatientWeight",
                                                                      "StudyID", "SeriesNumber", "AcquisitionNumber", "InstanceNumber",
                                                                      "BodyPartExamined", "SliceThickness", "SpacingBetweenSlices", "PixelSpacing", "PixelAspectRatio", "Rows", "Columns", "FieldOfViewDimensions",
                                                                      "RescaleIntercept", "RescaleSlope", "WindowCenter", "WindowWidth", "BitsAllocated", "BitsStored", "PhotometricInterpretation",
                                                                      "KVP", "ExposureTime", "XRayTubeCurrent", "ExposureInuAs", "ImageAndFluoroscopyAreaDoseProduct",
                                                                      "FilterType", "ConvolutionKernel", "CTDIvol", "ReconstructionFieldOfView"),
                          nThread = parallel::detectCores()-1, na = TRUE, identical = TRUE) {

  pydicom <- reticulate::import("pydicom", delay_load = TRUE)
  img_db  <- dcm_db(path = path, ext = ext, all = all, keywords = keywords, nThread = nThread, pydicom = pydicom)

  #Create pretty columns and convert values
  img_db$time_study             <- as.POSIXct(paste0(img_db$StudyDate, img_db$StudyTime), format = "%Y%m%d %H%M%S", tz = "EST")
  img_db$time_series            <- as.POSIXct(paste0(img_db$SeriesDate, img_db$SeriesTime), format = "%Y%m%d %H%M%S", tz = "EST")
  img_db$time_acquisition       <- as.POSIXct(paste0(img_db$AcquisitionDate, img_db$AcquisitionTime), format = "%Y%m%d %H%M%S", tz = "EST")
  img_db$name_img               <- trimws(gsub("[[:punct:]]", " ", img_db$PatientName))
  img_db$time_date_of_birth_img <- as.POSIXct(img_db$PatientBirthDate, format = "%Y%m%d", tz = "EST")
  img_db$img_pixel_spacing      <- as.numeric(gsub("\\[(.+),.*", "\\1", img_db$PixelSpacing))
  #img_db$img_pixel_xy_ratio     <- as.numeric(gsub("\\[(.+),.*", "\\1", img_db$PixelAspectRatio)) / as.numeric(gsub(".*,(.+)\\]", "\\1", d$PixelAspectRatio))

  img_db <- remove_column(dt = img_db, na = na, identical = identical)
  img_db
}
