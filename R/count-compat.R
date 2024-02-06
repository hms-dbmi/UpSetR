# This function aims to mimic plyr::count when vars or wt is not used.
# It would also be possible to use dplyr::count, but this avoids adding a new dependency 
# to the package
count_compat <- function(x) {
  if ("freq" %in% names(x)) warning("Used the weighting possibly (Internal)")
  res <- vctrs::vec_count(x, sort = "key")
  
  rownames(res) <- NULL
  
  cbind(res$key, freq = res$count)
}
