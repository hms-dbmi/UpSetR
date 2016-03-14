## Set metadata plot functions
metadataHist <- function(metadata,y_data){
  plot <- (ggplot(data=metadata)
                    + geom_bar(aes_string(x="sets", y=y_data),
                               stat="identity", position="identity", width = 0.4,
                               fill = "gray23")
                    + scale_x_continuous(limits = c(0.5, (nrow(metadata)+0.5)),
                                         breaks = c(0, max(metadata)),
                                         expand = c(0,0))
                    + theme(panel.background = element_rect("white"),
                            plot.margin=unit(c(-0.11,-0.3,0.5,0.5), "lines"),
                            axis.title.x = element_text(size = 11),
                            axis.line = element_line(colour = "gray0"),
                            axis.line.y = element_blank(),
                            axis.line.x = element_line(colour = "gray0", size = 0.3),
                            axis.text.y = element_blank(),
                            axis.ticks.y = element_blank(),
                            panel.grid.minor = element_blank(),
                            panel.grid.major = element_blank())
                    + xlab(NULL)
                    + coord_flip()
                    +scale_y_reverse())
  
  return(plot)
}