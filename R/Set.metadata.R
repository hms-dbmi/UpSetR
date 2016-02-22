Make_set_metadata_plot <- function(metadata, set_names){
  metadata <- as.data.frame(metadata)
  metadata_columns <- colnames(metadata)
  check <- rep(TRUE, length(set_names))
  setcol <- which(unname(apply(metadata,2,
               function(x) {
                 x <- set_names %in% x;
                 if (identical(x,check)) {
                   x <- TRUE
                 }
                 else{
                   x <- FALSE
                 }
               }))==TRUE)
  
  set_column <- names(metadata[setcol])
  set_column <- match(set_column, metadata_columns)
  metadata_columns[set_column] <- "sets"
  names(metadata) <- metadata_columns
  metadata <- metadata[which(metadata$sets %in% set_names), ]
  metadata <- metadata[order(set_names), ]
  metadata$sets <- seq(1,nrow(metadata))
  rownames(metadata) <- set_names
  
  y_data_name <- names(metadata[2])
  colnames(metadata) <- c("sets", "y")
  metadata$y <- as.numeric(as.character(metadata$y))
  

  metadata_plot <- (ggplot(data=metadata, aes_string(x="sets", y="y"))
                    + geom_bar(stat="identity", position="identity", width = 0.4,
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
                    +ylab(y_data_name)
                    + xlab(NULL)
                    + coord_flip()
                    +scale_y_reverse())
  
  metadata_plot <- ggplot_gtable(ggplot_build(metadata_plot))
  
return(metadata_plot)
}