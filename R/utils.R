#' Internal check: ensure required columns exist
#' @keywords internal
check_input_data <- function(data,
                             need_ah = FALSE,
                             need_odds = FALSE) {
  stopifnot(is.data.frame(data))

  base_cols <- c("league", "date", "home", "away", "hg", "ag")

  ah_cols <- c("ah_open", "ah_close")

  odds_cols <- c("odds_home_open", "odds_away_open",
                 "odds_home_close", "odds_away_close")

  need <- base_cols
  if (need_ah)   need <- c(need, ah_cols)
  if (need_odds) need <- c(need, odds_cols)

  missing <- setdiff(need, names(data))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }
}


#' Compute Asian Handicap outcome (home team perspective)
#' @keywords internal
compute_ah_outcome <- function(hg, ag, handicap) {
  adj <- hg - ag + handicap

  outcome <- character(length(adj))

  outcome[adj > 0.5]  <- "win"
  outcome[adj == 0.5] <- "half_win"
  outcome[adj == 0]   <- "push"
  outcome[adj == -0.5] <- "half_lose"
  outcome[adj < -0.5] <- "lose"

  return(outcome)
}
