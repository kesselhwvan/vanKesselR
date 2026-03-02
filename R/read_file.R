#' Read Data Files
#'
#' Reads data from various file formats (txt, csv, tsv, rds, fst, xlsx) with
#' optimized settings for tabular data. Text-based formats (txt, csv, tsv) are
#' read using [data.table::fread()] with multi-threading support.
#'
#' @param path A string specifying the file path. Must be a single existing file.
#'
#' @param type A character vector of supported file types. Allowed values are:
#'   "txt", "csv", "tsv", "rds", "fst", or "xlsx". Default includes all
#'   supported types. An error is raised if unsupported types are specified.
#'
#' @param ... Additional arguments passed to the underlying read function:
#'   - For xlsx files: passed to [readxl::read_xlsx()]
#'   - For other formats: ignored
#'
#' @returns A data frame containing the file data.
#'
#' @details
#' For text-based files (txt, csv, tsv), [data.table::setDTthreads()] is
#' configured to use `detectCores() - 1` threads for improved performance.
#' Empty strings are treated as `NA`.
#'
#' @examples
#' \dontrun{
#'   # Read a CSV file
#'   df <- read_file("data.csv")
#'
#'   # Read an Excel file with specific sheet
#'   df <- read_file("data.xlsx", sheet = "Sheet1")
#'
#'   # Read an RDS file
#'   df <- read_file("data.rds")
#' }
#'
#' @seealso
#'   [data.table::fread()] for CSV/TSV reading options
#'   [readxl::read_xlsx()] for Excel reading options
#'
#' @export
read_file <- function(path = NULL, type = c("txt", "csv", "tsv", "rds", "fst", "xlsx"), ...){

  stopifnot(is.character(path), length(path) == 1L, !is.na(path), nzchar(trimws(path)), file.exists(path))

  ext <- tools::file_ext(tolower(path))

  if (!all(type %in% c("txt", "csv", "tsv", "rds", "fst", "xlsx"))) cli::cli_abort(message = "Unsupported type provided")

  if (any(ext %in% c("txt", "csv", "tsv"))){
    return(data.table::fread(file = path, na.strings = "", showProgress = FALSE, data.table = FALSE, encoding = "UTF-8"))
  }

  if (ext == "rds") return(readRDS(file = path))
  if (ext == "fst") return(fst::read_fst(path = path))
  if (ext == "xlsx") return(readxl::read_xlsx(path = path, ...))
}

