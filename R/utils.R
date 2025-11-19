#' Internal utility: check required columns
#' @keywords internal
check_input_data <- function(data) {
  if (!is.data.frame(data)) stop("Data must be a data.frame.")
}
