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
                    + scale_y_reverse())
  
  return(plot)
}

metadataHeat <- function(metadata, y_data, plot_type){
  colnum <- match(y_data, names(metadata))
  names(metadata)[colnum] <- "current"
  if(is.factor(metadata$current) == TRUE){
    levs <- levels(metadata$current)
    metadata$current <- as.character(metadata$current)
    for(i in seq(length(levs))){
      metadata$current[which(metadata$current == levs[i])] <- i
    }
    metadata$current <- as.numeric(metadata$current)
  }
  if(is.character(metadata$current) == TRUE){
    warning("One of the columns used to generate a heat map was entered as a character vector.")
    warning("This may produce wrong results due to how the levels are generated.")
    warning("Change all heat map columns to numeric (if applicable) or factors.
            Make sure the levels of the factor is in the correct order")
    metadata$current <- as.factor(metadata$current)
    levs <- levels(metadata$current)
    metadata$current <- as.character(metadata$current)
    for(i in seq(length(levs))){
      metadata$current[which(metadata$current == levs[i])] <- i
    }
    metadata$current <- as.numeric(metadata$current)
  }
  
  names(metadata)[colnum] <- y_data
  
  if(nchar(y_data) > 5){
  titleAdjustment <- nchar(y_data)-5
  titleAdjustment <- 41 + titleAdjustment*6
  }
  else{
    titleAdjustment <- 40
  }

  plot <- (ggplot(data=metadata)
           + geom_raster(aes_string(x="sets", y=1, fill = y_data))
           + scale_x_continuous(limits = c(0.5, (nrow(metadata)+0.5)),
                                expand = c(0,0))
           + theme(panel.background = element_rect("white"),
                   plot.title = element_text(margin = margin(b=titleAdjustment), hjust = 0,
                                             size = 9.25, angle = 90),
                   plot.margin=unit(c(-0.11,-0.3,0.5,0.5), "lines"),
                   axis.title.x = element_text(size = 11),
                   legend.position = "none",
                   axis.line = element_blank(),
                   axis.line = element_line(colour = "gray0"),
                   axis.line.y = element_blank(),
                   axis.line.x = element_line(colour = "gray0", size = 0.3),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.title.y = element_blank(),
                   axis.text.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.title.x = element_blank())
           + ylab(NULL)
           + xlab(NULL)
           + ggtitle(y_data)
           + coord_flip()
           + scale_y_reverse())
  
  if(plot_type == "heat"){
    plot <- plot + scale_fill_gradient(low="grey86", high = "grey16")
  }
  else if(plot_type == "bool"){
    plot <- plot + scale_fill_gradient(low="white", high = "grey16")
  }
           
  return(plot)
}

metadataText <- function(metadata, y_data){
  ncols <- ncol(metadata)
  metadata <- cbind(metadata, c(1:nrow(metadata)))
  names(metadata)[ncol(metadata)] <- "x"
  plot <- (ggplot(data=metadata, aes_string(x="x", y=1, label = y_data))
           + scale_x_continuous(limits = c(0.5, (nrow(metadata)+0.5)),
                                expand = c(0,0))
           + theme(panel.background = element_rect("white"),
                   plot.margin=unit(c(-0.11,-0.3,0.5,0.5), "lines"),
                   axis.title.x = element_text(size = 11),
                   legend.position = "none",
                   axis.line = element_blank(),
                   axis.line = element_line(colour = "gray0"),
                   axis.line.y = element_blank(),
                   axis.line.x = element_line(colour = "gray0", size = 0.3),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.title.y = element_blank(),
                   axis.text.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.title.x = element_blank())
           + geom_text(size = 3)
           + xlab(NULL)
           + ylab(NULL)
           + coord_flip()
           + scale_y_reverse())
}