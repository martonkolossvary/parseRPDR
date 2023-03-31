#' @title Exports free text notes to individual text files.
#' @export
#'
#' @description Exports out the contents of a given cell per row into individual text files. Can be used to export out
#' reports into individual text files for further analyses.
#'
#' @param d data.table, database containing notes loaded using the \emph{load_notes} function. Theoretically any other data.table can be given
#' and the contents of the specified cell will be exported into the corresponding files. In case of notes, it is advised to load them
#' with \emph{format_orig = TRUE}, as then the output will retain the original format of the report making it easier to read.
#' @param folder string, full folder path to folder where the files should be exported. If folder does not exist, the function stops.
#' @param code string vector, column name containing the data that should be exported. Generally should be \emph{"abc_rep_txt"}, where \emph{abc} stands for the three letter abbreviation of the given type of note.
#' @param name1 string, the first part of the file names. Defaults to \emph{ID_MERGE}.
#' @param name2 string, the second part of the file names. name1 and name2 will be separated using "_".
#' Generally should be \emph{"abc_rep_num"}, where \emph{abc} stands for the three letter abbreviation of the given type of note.
#'
#' @return NULL, files are exported to given folder.
#'
#' @encoding UTF-8
#'
#' @examples \dontrun{
#' #Output all cardiology notes to given folder
#' d <- load_notes("Car.txt", type = "car", nThread = 2, format_orig = TRUE)
#' export_notes(d, folder = "/Users/Test/Notes/", code = "car_rep_txt",
#' name1 = "ID_MERGE", name2 = "car_rep_num")
#' }

export_notes <- function(d, folder, code, name1 = "ID_MERGE", name2) {
  .SD=.N=.I=.GRP=.BY=.EACHI=..=..cols=.SDcols=i=j=time_to_db=..which_ids_to=..which_ids_from=..collapse <- NULL

  message(paste0("Exporting ", code, " column information into seperate text files."))
  #Export files
  for(i in 1:dim(d)[1]) {
    fileConn <- file(paste0(folder, d[i, ][[name1]], "_", d[i, ][[name2]], ".txt"))
    writeLines(d[i, ][[code]], fileConn)
    close(fileConn)
  }
  message(paste0("Files written to: ", folder, "."))
}
