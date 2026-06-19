# matching plyr::count behavior. this is non-trivial to do with
#   pure base R; see PR description for full details.
#' @importFrom dplyr all_of arrange n pick summarize
plyr_count <- function(x) {
  cols <- names(x)
  arrange(summarize(x, .by = all_of(cols), freq = n()), pick(cols))
}
