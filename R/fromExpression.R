#' Expression to UpSetR converters 
#' 
#' @description A function to convert an expression to a data frame compatible with UpSetR.
#' @param input An vector (expression) to be converted to an input compatible with UpSetR
#' @note See "Basic Usage" vignette for an example on how to use this function in UpSetR.
#' @export 
fromExpression <- function(input){
  input <- list(input)
  intersections <- lapply(input, function(x) strsplit(names(unlist(x)), "&"))
  intersections <- lapply(intersections[[1]], function(x) unlist(as.list(x)))
  sets <- unique(unlist(intersections))
  data <- na.omit(data.frame(matrix(NA, ncol = length(sets))))
  names(data) <- sets
  counts <- lapply(input, function(x) unlist(x))
  names(counts[[1]]) <- NULL
  counts[[1]] <- as.numeric(counts[[1]])
  
  for(i in seq(intersections)) {
    cols <- match(names(data), intersections[[i]])
    cols[!is.na(cols)] <- 1
    cols[is.na(cols)] <- 0
    cols <- rep(cols, times = counts[[1]][i])
    cols <- matrix(cols, ncol = length(sets), byrow = T)
    cols <- data.frame(cols)
    names(cols) <- sets
    data <- rbind(data, cols)
  }
  return(data)
}