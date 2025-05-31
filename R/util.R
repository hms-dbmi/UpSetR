# matching plyr::count behavior. this is non-trivial to do with
#   pure ; see PR description for full details.
#' @importFrom dplyr n summarize
plyr_count <- function(x) summarize(x, .by = names(x), freq = n())
