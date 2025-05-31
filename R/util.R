# matching plyr::count behavior. this is non-trivial to do with
#   pure ; see PR description for full details. for three main reasons:
#   1. table(<data.frame>) may fail if there are very few
#      duplicates and each column is of high cardinality, meaning
#      table(x) would have a very large number of 0 entries that
#      need to be computed and dropped (plyr::count skips them).
#   2. We can use something like interaction(..., drop=TRUE) + 
#      tapply() to imitate this, but it's hard to generically
#      reconstruct the un-interacted levels needed to build an
#      equivalent data.frame -- basically, we'd need to, for full
#      generality, use a sep=<str> where <str> is not present in
#      any of the unique values of any of the columns of x in order
#      for strsplit(<level>, <sep>) to uniquely map back.
#   3. Something like vapply(split(x, x), nrow, integer(1L)) is also
#      appealingly simple, _but_ split() always drops missing levels
#      (https://bugs.r-project.org/show_bug.cgi?id=18899) --> we'd
#      need an onerous/ugly loop over the columns to replace missing
#      observations with a unique NA-equivalent, end-sorting sentinel.
#' @importFrom dplyr n summarize
plyr_count <- function(x) summarize(x, .by = names(x), freq = n())
