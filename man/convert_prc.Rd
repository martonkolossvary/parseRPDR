% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_prc.R
\encoding{UTF-8}
\name{convert_prc}
\alias{convert_prc}
\title{Searches procedures columns for given procedures.}
\usage{
convert_prc(
  d,
  code = "prc_code",
  code_type = "prc_code_type",
  codes_to_find = NULL,
  collapse = NULL,
  code_time = "time_prc",
  aggr_type = "earliest",
  nThread = parallel::detectCores() - 1
)
}
\arguments{
\item{d}{data.table, database containing procedure information data loaded using the \emph{load_prc} function.}

\item{code}{string, column name of the procedure code column. Defaults to \emph{prc_code}.}

\item{code_type}{string, column name of the code_type column. Defaults to \emph{prc_code_type}.}

\item{codes_to_find}{list, a list of string arrays corresponding to sets of code types and codes separated by \emph{:}, i.e.: "CPT:00104".
The function searches for the given procedure code type and code pair and adds new boolean columns with the name of each list element.
These columns are indicators whether any of the procedure code type and code pair occurs in the set of codes.}

\item{collapse}{string, a column name on which to collapse the data.table.
Used in case we wish to assess multiple procedure codes are present within all the same instances of \emph{collapse}. See vignette for details.}

\item{code_time}{string, column name of the time column. Defaults to \emph{time_prc}. Used in case collapse is present to provide the earliest or latest instance of the given procedure.}

\item{aggr_type}{string, if multiple procedures are present within the same case of \emph{collapse}, which timepoint to return. Supported are: "earliest" or "latest". Defaults to \emph{earliest}.}

\item{nThread}{integer, number of threads to use for parallelization. If it is set to 1, then no parallel backends are created and the function is executed sequentially.}
}
\value{
data.table, with indicator columns whether the any of the given procedures are reported.
If \emph{collapse} is present, then only unique ID and the summary columns are returned.
}
\description{
Analyzes procedure data loaded using \emph{load_prc}. Searches procedures columns for a specified set of procedures.
By default, the data.table is returned with new columns corresponding to boolean values, whether given group of procedures are present in the given procedure.
If \emph{collapse} is given, then the information is aggregated based-on the \emph{collapse} column and the earliest of latest time of the given procedure is provided.
}
\examples{
\dontrun{
#Search for Anesthesia CPT codes
procedures <- list(Anesthesia = c("CTP:00410", "CPT:00104"))
data_prc_parse <- convert_prc(d = data_prc, codes_to_find = procedures, nThread = 2)

#Search for Anesthesia CPT codes
procedures <- list(Anesthesia = c("CTP:00410", "CPT:00104"))
data_prc_procedures <- convert_prc(d = data_prc, codes_to_find = procedures,
nThread = 2, collapse = "ID_MERGE", aggr_type = "earliest")
}
}
