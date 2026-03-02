#' Read a single file into R based on its extension
#'
#' @description
#' Reads a single file from disk and returns its contents. The file type is inferred from the
#' filename extension. Tabular text files are read with \code{data.table::fread()}, RDS files
#' with \code{readRDS()}, FST files with \code{fst::read_fst()}, and Excel files with
#' \code{readxl::read_xlsx()}. Optional post-processing converts data frames to tibbles and
#' optionally standardizes column names.
#'
#' @param path Character scalar. Path to an existing file.
#' @param type Character vector. Allowed file types (extensions). Supported values are
#'   \code{"txt"}, \code{"csv"}, \code{"tsv"}, \code{"rds"}, \code{"fst"}, \code{"xlsx"}.
#'   This parameter validates allowed extensions; the actual reader is chosen by the file's
#'   extension extracted from \code{path}.
#' @param df_as_tibble Logical scalar. If \code{TRUE} and the imported object inherits from
#'   \code{"data.frame"}, converts it to a tibble via \code{tibble::as_tibble()}. If row names
#'   exist, they are preserved in a column named \code{"rowname"}.
#' @param clean_names Logical scalar. If \code{TRUE} and the imported object inherits from
#'   \code{"data.frame"}, cleans column names using \code{janitor::clean_names()}.
#' @param ... Additional arguments forwarded to \code{readxl::read_xlsx()} when reading
#'   \code{.xlsx} files. Ignored for other file types.
#'
#' @return
#' An R object produced by the relevant file reader. For tabular inputs, this is typically a
#' \code{data.frame} or tibble depending on \code{df_as_tibble}. For \code{.rds} it is the
#' stored R object. For \code{.fst} it is the object returned by \code{fst::read_fst()}.
#'
#' @details
#' The function validates that \code{path} exists and that \code{type} is restricted to a known
#' set of extensions. The read method is selected solely via \code{tools::file_ext(path)}.
#' If \code{clean_names = TRUE}, column-name cleaning occurs after optional tibble conversion.
#'
#' @seealso
#' \code{\link[tools]{file_ext}}, \code{\link[data.table]{fread}}, \code{\link[base]{readRDS}},
#' \code{\link[fst]{read_fst}}, \code{\link[readxl]{read_xlsx}}, \code{\link[tibble]{as_tibble}},
#' \code{\link[janitor]{clean_names}}
#'
#' @examples
#' \dontrun{
#' d1 <- read_file("data/example.csv")
#' d2 <- read_file("data/example.xlsx", sheet = 2)
#' m  <- read_file("models/fit.rds", df_as_tibble = FALSE)
#' }
#'
#' @export
read_file <- function(path = NULL, type = c("txt", "csv", "tsv", "rds", "fst", "xlsx"), df_as_tibble = TRUE, clean_names = TRUE, ...){
  # path
  stopifnot(is.character(path),
            length(path) == 1L,
            !is.na(path),
            nzchar(trimws(path)),
            file.exists(path))
  # type
  stopifnot(is.character(type),
            length(type) > 0L,
            all(!is.na(type)),
            all(nzchar(trimws(type))),
            all(type %in% c("txt", "csv", "tsv", "rds", "fst", "xlsx")))
  # as_tibble
  stopifnot(is.logical(df_as_tibble),
            length(df_as_tibble) == 1L,
            !is.na(df_as_tibble))
  # clean_names
  stopifnot(is.logical(clean_names),
            length(clean_names) == 1L,
            !is.na(clean_names))
  # extension
  ext <- tolower(tools::file_ext(path))
  # read based on extension
  if (ext %in% c("txt", "csv", "tsv")) obj <- data.table::fread(file = path, na.strings = "", data.table = FALSE)
  if (ext == "rds") obj <- readRDS(file = path)
  if (ext == "fst") obj <- fst::read_fst(path = path)
  if (ext == "xlsx") obj <- readxl::read_xlsx(path = path, ...)
  # execute df_as_tibble
  if (inherits(obj, "data.frame") && isTRUE(df_as_tibble)) obj <- tibble::as_tibble(obj, rownames = if (tibble::has_rownames(obj)) "rowname" else NULL)
  # execute clean_names
  if (inherits(obj, "data.frame") && isTRUE(clean_names)) obj <- janitor::clean_names(obj)
  # return obj
  return(obj)
}
