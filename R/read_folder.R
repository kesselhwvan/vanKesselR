#' Read multiple files from a folder into a named list (optionally attach to an environment)
#'
#' @description
#' Reads all files in a directory whose extensions match \code{type}. Each file is read via
#' \code{read_file()} and returned as a named list. Names are derived from the lowercased
#' base filename without extension, made unique if duplicates occur. Optionally assigns the
#' objects into \code{envir} and returns the object names invisibly.
#'
#' @param path Character scalar. Path to an existing directory.
#' @param attach Logical scalar. If \code{TRUE}, assigns the imported objects into \code{envir}
#'   and returns the names invisibly. If \code{FALSE}, returns a named list.
#' @param type Character vector. File types (extensions) to include. Supported values are
#'   \code{"txt"}, \code{"csv"}, \code{"tsv"}, \code{"rds"}, \code{"fst"}, \code{"xlsx"}.
#'   Matching is case-insensitive.
#' @param df_as_tibble Logical scalar. Passed to \code{read_file()} to control whether data
#'   frames are returned as tibbles.
#' @param clean_names Logical scalar. Passed to \code{read_file()} to control whether column
#'   names are cleaned.
#' @param envir Environment. Target environment used when \code{attach = TRUE}. Defaults to
#'   \code{parent.frame()}.
#' @param ... Additional arguments forwarded to \code{read_file()}.
#'
#' @return
#' If \code{attach = FALSE}, a named list of imported objects. If \code{attach = TRUE},
#' the character vector of object names is returned invisibly.
#'
#' @details
#' Files are detected with \code{list.files()} using a regular expression built from \code{type}.
#' Object names are computed as \code{make.unique(file_path_sans_ext(tolower(basename(file))))}.
#' The function errors if \code{path} is not a valid existing directory or if inputs fail
#' basic type and length checks.
#'
#' @seealso
#' \code{\link[base]{list.files}}, \code{\link[base]{list2env}}
#'
#' @examples
#' \dontrun{
#' x <- read_folder("data", type = c("csv", "xlsx"))
#' read_folder("data", attach = TRUE, envir = .GlobalEnv)
#' }
#'
#' @export
read_folder <- function(path = NULL,
                        attach = FALSE,
                        type = c("txt", "csv", "tsv", "rds", "fst", "xlsx"),
                        df_as_tibble = TRUE,
                        clean_names = TRUE,
                        envir = parent.frame(),
                        ...){
  # path
  stopifnot(is.character(path),
            length(path) == 1L,
            !is.na(path),
            dir.exists(path))
  # attach
  stopifnot(is.logical(attach),
            length(attach) == 1L,
            !is.na(attach))
  # type
  stopifnot(is.character(type),
            length(type) > 0L,
            all(!is.na(type)),
            all(nzchar(trimws(type))),
            all(type %in% c("txt", "csv", "tsv", "rds", "fst", "xlsx")))
  # df_as_tibble
  stopifnot(is.logical(df_as_tibble),
            length(df_as_tibble) == 1L,
            !is.na(df_as_tibble))
  # clean_names
  stopifnot(is.logical(clean_names),
            length(clean_names) == 1L,
            !is.na(clean_names))
  # envir
  stopifnot(is.environment(envir))
  # file names in directory
  name <- list.files(path = path,
                     pattern = paste0("\\.(", paste(tolower(type), collapse = "|"), ")$"),
                     full.names = TRUE,
                     ignore.case = TRUE)
  # read files
  obj <- purrr::map(.x = stats::setNames(object = name,
                                         nm = make.unique(tools::file_path_sans_ext(tolower(basename(name))), sep = "")),
                    .f = ~read_file(.x, type = type, df_as_tibble = df_as_tibble, clean_names = clean_names, ...))
  # return obj
  if (isTRUE(attach)){
    list2env(x = obj,
             envir = envir)
    invisible(names(obj))
  } else {
    return(obj)
  }
}
