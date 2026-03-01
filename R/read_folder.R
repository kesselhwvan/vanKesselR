read_folder <- function(path = NULL, attach = FALSE, type = c("txt", "csv", "tsv", "rds", "fst", "xlsx"), ...){

  stopifnot(length(path) == 1L, is.character(path), dir.exists(path))
  stopifnot(is.logical(attach), length(attach) == 1L, !is.na(attach))
  stopifnot(length(type) > 0L, is.character(type))

  object <- list.files(
    path = path,
    pattern = paste0("\\.(", paste(tolower(type), collapse = "|"), ")$"),
    full.names = TRUE,
    ignore.case = TRUE
  )

  res <- purrr::map(
    .x = setNames(object = object,
                  nm = make.unique(tools::file_path_sans_ext(tolower(basename(object))), sep = "")),
    .f = ~read_file(.x, type = type, ...)
  )

  if (attach){
    list2env(res, envir = .GlobalEnv)
    invisible(names(res))
  } else {
    res
  }
}


read_folder(path = "test", attach = T)
# read_folder(path = "test", type = c("csv", "txt", "rds", "aa"))
