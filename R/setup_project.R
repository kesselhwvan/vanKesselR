# setup_project <- function(folder, git = TRUE, open = FALSE){
#
#   stopifnot(is.character(folder),
#             length(folder) == 1L,
#             !is.na(folder))
#
#   stopifnot(is.logical(open),
#             length(open) == 1L,
#             !is.na(open))
#
#   folder <- fs::path_abs(folder)
#
#   if (!dir.exists(folder)) {
#     dir.create(folder, recursive = TRUE, showWarnings = FALSE)
#   }
#
#   if (length(list.files(folder, all.files = TRUE, no.. = TRUE)) > 0L) {
#     stop("New project can only be created in an empty folder.", call. = FALSE)
#   }
#
#   rstudio_available <- requireNamespace("rstudioapi", quietly = TRUE) &&
#     rstudioapi::isAvailable()
#
#   # create project
#   usethis::create_project(
#     path = folder,
#     rstudio = rstudio_available,
#     open = open
#   )
#
#   # ensure the project is the active usethis target in this session
#   usethis::proj_activate(folder)
#
#   # create subdirectories
#   purrr::walk(
#     c("data", "figure"),
#     ~ dir.create(file.path(folder, .x), recursive = TRUE, showWarnings = FALSE)
#   )
#
#   # initialise git for the active project
#   if (git) {
#     usethis::use_git()
#   }
#
#   # create README in the active project
#   usethis::use_readme_md(open = FALSE)
#
#   fs::dir_tree(path = folder, all = TRUE)
# }



# setup_project(folder = "C:/Users/hwvan/Documents/projects/new")


# getwd()
