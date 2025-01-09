#' Generate file path to import raw calval data
#'
#' Raw data must be saved in a folder path/validation_data/valXXX or
#' path/validation_data/postXXX.
#'
#' @param path File path to the validation_data folder on the CMAR R drive.
#'
#' @param val_id Character string of the validation id. Will be converted to
#'   lower case, and all spaces will be replaced with an underscore.
#'
#' @param check_path Logical parameter indicating whether to check if the path
#'   exists. Default is /code{TRUE}.
#'
#' @return The file path for importing raw calval data.
#'
#' @importFrom stringr str_replace_all
#'
#' @export

cv_import_path <- function(val_id, path = NULL, check_path = TRUE) {
  if (is.null(path)) {
    path <- "R:/data_branches/water_quality/validation/validation_data"
  }

  val_id <- tolower(val_id)
  #val_id <- str_replace_all(val_id, " ", "_")

  path <- paste(path, val_id, sep = "/")

  if (isTRUE(check_path) & isFALSE(dir.exists(path))) {
    stop("File path << ", path, " >> does not exist. Check val_id")
  }

  file.path(path)
}
