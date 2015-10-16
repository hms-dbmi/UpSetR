#' List of named vectors to UpSetR converter
#' 
#' @description A function to convert a list of named vectors to a data frame compatible with UpSetR.
#' @param input A list of named vectors to be converted to a data frame compatible with UpSetR
#' @note See "Basic Usage" vignette for an example on how to use this function in UpSetR.
#' @export 
fromList <- function(input){
  elements <- unique(unlist(input))
  data <- unlist(lapply(input, function(x){x <- as.vector(match(elements, x))}))
  data[is.na(data)] <- as.integer(0); data[data != 0] <- as.integer(1)
  data <- data.frame(matrix(data, ncol = length(input), byrow = F))
  data <- data[which(rowSums(data) !=0), ]
  names(data) <- names(input)
  return(data)
}