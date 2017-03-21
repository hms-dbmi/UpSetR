## Set metadata plot functions
metadataHist <- function(metadata, y_data, colors){
  colnum <- match(y_data, names(metadata))
  names(metadata)[colnum] <- "current"
  if(is.numeric(metadata$current) == FALSE){
    warning("The values supplied for the metadata histogram were not numeric")
  }
  metadata <- metadata[c(1,colnum)]
  if(is.factor(metadata$current) == TRUE){
    warning("The data being used for the bar plot is not numeric!")
  }
  names(metadata)[colnum] <- y_data
  
  if(is.null(colors)){
    colors <- "gray23"
  }
  
  plot <- (ggplot(data=metadata)
           + geom_bar(aes_string(x="sets", y=y_data),
                      stat="identity", position="identity", width = 0.4,
                      fill = colors)
           + scale_x_continuous(limits = c(0.5, (nrow(metadata)+0.5)),
                                breaks = c(0, max(metadata)),
                                expand = c(0,0))
           + theme(panel.background = element_rect("white"),
                   plot.margin=unit(c(0,0,0,0), "lines"),
                   axis.title.x = element_text(size = 8.3),
                   axis.text.x = element_text(size = 7),
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

metadataHeat <- function(metadata, y_data, plot_type, colors){
  palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
               "#CC79A7")
  colnum <- match(y_data, names(metadata))
  names(metadata)[colnum] <- "current"
  if(is.factor(metadata$current) == TRUE){
    colortype <- "factor"
    levs <- levels(metadata$current)
    if(plot_type == "bool"){
      newlevel <- c(0,1)
    }
    else if(plot_type == "heat"){
      newlevel <- c(1:length(levs))
    }
    metadata$current <- as.character(metadata$current)
    for(i in seq(length(levs))){
      metadata$current[which(metadata$current == levs[i])] <- newlevel[i]
    }
    metadata$current <- as.numeric(metadata$current)
  }
  else if(is.character(metadata$current) == TRUE){
    colortype <- "category"
    uniquecats <- length(unique(metadata$current))
  }
  else if(is.numeric(metadata$current) == TRUE){
    if(plot_type != "bool"){
    colortype <- "factor"
    }
    else if(plot_type == "bool"){
      metadata$current <- as.character(metadata$current)
      colortype = "category"
    }
  }
  
  names(metadata)[colnum] <- y_data
  
  # if(nchar(y_data) > 5){
  #   titleAdjustment <- nchar(y_data)-5
  #   titleAdjustment <- 41 + titleAdjustment*6
  # }
  # else{
    titleAdjustment <- 25
  #}
  
  plot <- (ggplot(data=metadata, aes_string(x="sets", y = 1, fill = y_data))
           + scale_x_continuous(expand = c(c(0,0), c(0,0)))
           + theme(panel.background = element_rect("white"),
                   plot.title = element_text(margin = margin(b=titleAdjustment),
                                             size = 9, hjust = 0.5),
                   plot.margin=unit(c(0,0,0,0), "lines"),
                   axis.title.x = element_text(size = 8.3),
                   legend.position = "none",
                   axis.line = element_blank(),
                   axis.line.y = element_blank(),
                   axis.line.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.title.y = element_blank(),
                   axis.text.x = element_blank(),
                   axis.ticks.x = element_blank())
           + ylab(NULL)
           + xlab(NULL)
           + ggtitle(y_data)
           + coord_flip()
           + scale_y_reverse())
  
  if(plot_type == "heat" && colortype == "factor"){
    plot <- plot + geom_tile()
    plot <- plot + scale_fill_gradient(low="grey86", high = "grey16")
  }
  else if(plot_type == "heat" && colortype == "category"){
    if(is.null(colors) && uniquecats < 9){
      colors <- palette[c(1:uniquecats)]
      plot <- plot + geom_tile()
      plot <- plot + scale_fill_manual(values=colors)
    }
    
    else if(is.null(colors)  && nrow(metadata) >= 9){
      warning("Please provide color palette when number of groups exceeds 8")
    }
    
    else if(is.null(colors) == FALSE){
      plot <- plot + geom_tile()
      plot <- plot + scale_fill_manual(values = colors)
    }
    
  }
  else if(plot_type == "bool" && colortype == "factor"){
    plot <- plot + geom_tile()
    plot <- plot + scale_fill_gradient(low="white", high = "grey16")
  }
  else if(plot_type == "bool" && colortype == "category"){
    if(is.null(colors)){
      plot <- plot + geom_tile()
      plot <- plot + scale_fill_manual(values  = c("white", "grey16"))
    }
    else{
      plot <- plot + geom_tile()
      plot <- plot + scale_fill_manual(values = c("0" = colors[1], "1" = colors[2]))
    }
  }
  
  return(plot)
}

metadataText <- function(metadata, y_data, colors, alignment){
  if(is.null(alignment) || alignment == "center"){
    align <- 0.5
  }
  else if(alignment == "right"){
    align <- 1
  }
  else if(alignment == "left"){
    align <- 0
  }
  
  # if(nchar(y_data) > 5){
  #   titleAdjustment <- nchar(y_data)-5
  #   titleAdjustment <- 41 + titleAdjustment*6
  # }
  #else{
    titleAdjustment <- 25
  #}
  
  ncols <- ncol(metadata)
  metadata <- cbind(metadata, c(1:nrow(metadata)))
  names(metadata)[ncol(metadata)] <- "x"
  plot <- (ggplot(data=metadata, aes_string(x="x", y=1, label = y_data, colour = y_data, size =10))
           + scale_x_continuous(limits = c(0.5, (nrow(metadata)+0.5)),
                                expand = c(0,0))
           + theme(panel.background = element_rect("white"),
                   plot.title = element_text(margin = margin(b=titleAdjustment),
                                             size = 9, hjust = 0.5),
                   plot.margin=unit(c(0,0,0,0), "lines"),
                   axis.title.x = element_text(size = 7),
                   legend.position = "none",
                   axis.line = element_blank(),
                   axis.line.y = element_blank(),
                   axis.line.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.title.y = element_blank(),
                   axis.text.x = element_blank(),
                   axis.ticks.x = element_blank())
           + xlab(NULL)
           + ylab(NULL)
           + coord_flip()
           + ggtitle(y_data)
           + scale_y_reverse())
  if(is.null(colors) == FALSE){
    plot <- plot + geom_text(size = 2.7, hjust = align)
    plot <- plot + scale_colour_manual(values = colors)
  }
  else{
    plot <- plot + geom_text(size = 2.7, hjust = align, colour = "gray23")
  }
  return(plot)
}

get_shade_groups <- function(set_metadata, set_names, Mat_data, shade_alpha) {
  data <- set_metadata$data
  names(data)[1] <- "sets"
  data <- data[which(data$sets %in% set_names), ]
  data <- data[match(set_names, data$sets), ]
  for (i in 1:length(set_metadata$plots)) {
    if (set_metadata$plots[[i]]$type == "matrix_rows") {
      col <- match(set_metadata$plots[[i]]$column, colnames(data))
      colors <- set_metadata$plots[[i]]$colors
      names(data)[col] <- "group"
      groups <- unique(data$group)
      data$color <- "gray88"
      for (j in 1:length(groups)) {
        data$color[which(data$group == names(colors)[j])] <- colors[[j]]
      }
      y <- c(1:length(set_names))
      shade_data <- data.frame(cbind(y))
      shade_data$min <- 0
      shade_data$max <- (max(Mat_data$x) + 1)
      for(k in 1:length(y)){
        shade_data$y_min[k] <- ((k) - 0.5)
        shade_data$y_max[k] <- ((k) + 0.5)
      }
      shade_data$shade_color <- data$color
      if(is.null(set_metadata$plots[[i]]$alpha) == TRUE){
        shade_data$alpha <- shade_alpha
      }
      else{
        shade_data$alpha <- set_metadata$plots[[i]]$alpha
      }
      return(shade_data)
    }
  }
}
