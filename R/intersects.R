#' Intersection query for queries parameter
#' 
#' @description A query paramter to visualize elements contained in specific intersections
#' @param func A functions provided internally
#' @param query Input from the queries parameter assigned to the intersection function.
#' @param ... Additional parameters to be applied internally 
#' @note See examples section of upset function on how to use this function in the queries parameter.
#' @export
intersects <- function(func, query, ...){
  data <- func(query,...)
  return(data)
}