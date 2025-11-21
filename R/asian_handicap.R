#' Historical Asian Handicap distribution
#'
#' Given a league and opening / closing Asian handicap lines, this function
#' finds historical matches with similar lines and summarizes the empirical
#' distribution of betting outcomes (from the home team perspective).
#'
#' @param league Character string, league identifier (e.g. "EPL").
#' @param open_line Numeric, opening AH line for the home team (e.g. -0.25).
#' @param close_line Numeric, closing AH line for the home team.
#' @param data Data frame with match information, including columns
#'   \code{league}, \code{hg}, \code{ag}, \code{ah_open}, \code{ah_close}.
#' @param tol Numeric tolerance when matching lines (default 0.05).
#'
#' @return A list with components:
#' \describe{
#'   \item{data}{Subset of matches used in the calculation.}
#'   \item{summary}{Data frame with outcome, count and empirical probability.}
#'   \item{n}{Number of matched games.}
#' }
#' @export
handicap_distribution <- function(league,
                                  open_line,
                                  close_line,
                                  data,
                                  tol = 0.05) {

  check_input_data(data, need_ah = TRUE)

  df <- subset(
    data,
    league == league &
      abs(ah_open - open_line) <= tol &
      abs(ah_close - close_line) <= tol
  )

  if (nrow(df) == 0) {
    stop("No matched historical games.")
  }

  df$outcome <- compute_ah_outcome(df$hg, df$ag, df$ah_close)

  tab  <- table(df$outcome)
  prob <- prop.table(tab)

  summary <- data.frame(
    outcome = names(tab),
    count   = as.numeric(tab),
    prob    = as.numeric(prob)
  )

  list(
    data    = df,
    summary = summary,
    n       = nrow(df)
  )
}
