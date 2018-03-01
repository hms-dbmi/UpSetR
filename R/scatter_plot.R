#' Scatterplot for customplot
#'
#' @description A pre-made scatter plot that can be added to the custom.plot parameter.
#' @param mydata A data set containing intersection data provided internally
#' @param x The x aesthetic for the scatter plot
#' @param y The y aesthetic for the scatter plot
#' @note See examples section for upset function on how to use custom.plot parameter.
#' @export
scatter_plot <- function(mydata, x, y){
  att_plot <- (ggplot(data = mydata, aes_string(x = x, y = y, colour = "color"))
               + geom_point(shape=16) + scale_color_identity()
               + theme(panel.background = element_rect(fill = "white"),
                       plot.title = element_text(vjust = 1.3),
                       panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       axis.title.y = element_text(vjust = 1.3, size = 8.3),
                       axis.title.x = element_text(size = 8.3),
                       plot.margin=unit(c(0.5,0,0,1), "cm")))
}
