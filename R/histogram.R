#' Histogram for custom plot
#'
#' @description A pre-made histogram that can be added to custom.plot parameter. 
#' @param mydata A data set containing intersection data provided internally
#' @param x The x aesthetic of for the histogram plot
#' @note See examples section for upset function on how to use custom.plot parameter
#' @export
histogram <- function(mydata, x){
  att_plot <- (ggplot(data = mydata, aes_string(x = x, fill = "color")) 
               + scale_fill_identity()
               + geom_histogram(binwidth = 1)
               + ylab("Frequency")
               + theme(panel.background = element_rect(fill = "white"),
                       plot.title = element_text(vjust = 1.5),
                       panel.border = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       axis.title.y = element_text(vjust = 1.3, size = 8.3),
                       axis.title.x = element_text(size = 8.3),
                       plot.margin=unit(c(0.5,0,0,1), "cm")))
}
