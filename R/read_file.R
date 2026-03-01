#' Read a file
#'
#' @param path a string
#'
#' @returns a table
#' @export
read_file <- function(path = NULL, type = c("txt", "csv", "tsv", "rds", "fst", "xlsx"), ...){

  stopifnot(length(path) == 1L, is.character(path), file.exists(path))
  ext <- tools::file_ext(tolower(path))
  supported <- c("txt", "csv", "tsv", "rds", "fst", "xlsx")

  if (!all(type %in% supported)) cli::cli_abort(message = "Unsupported type provided")

  if (any(ext %in% c("txt", "csv", "tsv"))){
    data.table::setDTthreads(max(1L, parallel::detectCores() - 1L))
    return(data.table::fread(file = path, na.strings = "", showProgress = FALSE, data.table = FALSE, encoding = "UTF-8"))
  }

  if (ext == "rds") return(readRDS(file = path))
  if (ext == "fst") return(fst::read_fst(path = path))
  if (ext == "xlsx") return(readxl::read_xlsx(path = path, ...))
}

