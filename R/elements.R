#' Element query for queries parameter
#' 
#' @description A query paramter to visualize specific elements of interest if queries = active (in custom plots only)
#' @param func A functions provided internally
#' @param query Input from the queries parameter assigned to the element function.
#' @param ... Additional parameters to be supplied internally 
#' @note See examples section of upset function on how to use this function in the queries parameter.
#' @export
elements <- function(func, query, ...){
  data <- func(query, ...)
  return(data)
}