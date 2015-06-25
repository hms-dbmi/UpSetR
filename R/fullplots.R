vplayout <- function(x,y){
  viewport(layout.pos.row = x, layout.pos.col = y)
}

NoAttBasePlot <- function(legend, size_plot_height, Main_bar_plot, Matrix_plot, hratios,
                          Size_plot, query_legend){
  top <- 1
  bottom <- 100
  if((is.null(legend) == F) && (query_legend != tolower("none"))){
    if(query_legend == tolower("top")){
    top <- 3
    bottom <- 102
    legend_top <- 1
    legend_bottom <- 3
    size_plot_height <-(size_plot_height + 2)
    }
    else if(query_legend == tolower("bottom")){
      legend_top <- 101
      legend_bottom <- 103
    }
  }
  grid.newpage()
  if((is.null(legend) == F) && (query_legend != tolower("none"))){
    if(query_legend == tolower("top")){
    pushViewport(viewport(layout = grid.layout(102,100)))
    }
    else if(query_legend == tolower("bottom")){
      pushViewport(viewport(layout = grid.layout(103, 100)))
    }
  }
  else if((is.null(legend) == T)|| (query_legend == tolower("none"))){
    pushViewport(viewport(layout = grid.layout(100,100)))
  }
  print(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios), vp = vplayout(top:bottom, 21:100))
  print(arrangeGrob(Size_plot), vp = vplayout(size_plot_height:bottom, 1:20))
  if((is.null(legend) == F) && (query_legend != tolower("none"))){
    print(arrangeGrob(legend), vp = vplayout(legend_top:legend_bottom, 21:100))
  }
}

HistoAttPlot <- function(att_x, att_y, Set_data, start_col, labels, exp, elems_att, q_att, att_color,
                         Q_Title, customQ, hratios, position, size_plot_height, legend,
                         Main_bar_plot, Matrix_plot, Size_plot, query_legend){
  col_to_switch <- match(att_x, colnames(Set_data))
  end_col <- ((start_col + as.integer(length(labels))) - 1)
  Set_data <- Set_data[which(rowSums(Set_data[ ,start_col:end_col]) != 0), ]
  #if(is.null(exp) == F){
  #  Set_data <- Subset_att(Set_data, exp)
  #}
  colnames(Set_data)[col_to_switch] <- "values"
  if(is.null(elems_att) == F){
    elems <- elems_att
    if(nrow(elems) != 0){
      elems <- elems[order(elems$val1), ]
      EColors <- unique(elems$color)
    }
    else{
      elems <- NULL
    }
  }
  else{
    elems <- NULL
  }
  if(is.null(q_att) == F){
    intersect <- q_att
    if(nrow(intersect) != 0){
      intersect <- intersect[order(intersect$v1), ]
      IColors <- unique(intersect$IColor)
    }
    else{
      intersect <- NULL
    }
  }
  else{
    intersect <- NULL
  }
  att_plot <- (ggplot(data = Set_data, aes(x = values)) 
               + geom_histogram(binwidth = 1, colour = "black", fill = att_color)
               + xlab(att_x) + ylab("Frequency") + labs(title = Q_Title)
               + theme(panel.background = element_rect(fill = "white"),
                       plot.title = element_text(vjust = 1.5),
                       panel.border = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       axis.title.y = element_text(vjust = -0.8),
                       plot.margin=unit(c(-0.7,0.2,0.1,0.2), "cm")))
  if(is.null(elems) == F){
    for(i in 1:length(EColors)){
      Color <- EColors[i]
      elems_data <- elems[which(elems$color == Color), ]
      att_plot <- att_plot + geom_histogram(data = elems_data, aes(x = val1), 
                                            binwidth = 1, colour = "black", fill = Color, alpha =0.5)
    }
  }
  if(is.null(intersect) == F){
    for(i in 1:length(IColors)){
      Color <- IColors[i]
      intersect_data <- intersect[which(intersect$IColor == Color ), ]
      att_plot <- att_plot + geom_histogram(data = intersect_data, aes(x = v1), binwidth = 1,
                                            colour = "black", fill = Color, alpha =0.5)
    }
  }
  if(length(customQ) != 0){
    col <- match(att_x, colnames(customQ))
    colnames(customQ)[col] <- "cval1"
    customQ <- customQ[order(customQ$cval1), ]
    CColor <- unique(customQ$color2)
    for( i in 1:length(CColor)){
      Color <- CColor[i]
      customQData <- customQ[which(customQ$color2 == Color), ]
      att_plot <- (att_plot + geom_histogram(data = customQData, aes(x=cval1),
                                             colour = "black", fill = Color, alpha = 0.5, binwidth = 1))
    }
  }
  
  att_plot <- ggplot_gtable(ggplot_build(att_plot))
  att_plot$widths <-  Matrix_plot$widths
  if((hratios[1] < 0.4) || 
       (hratios[2] > 0.6)) warning("Plot might be out of range if mb.ratio[1] < 0.4 or mb.ratio[2] >  0.6")
  if((is.null(position) == T) || (position == tolower("bottom"))){
    bar_top <- 1
    matrix_bottom <- 100
    att_top <- 101
    att_bottom <- 130
    size_plot_height <- (((hratios[1])+0.01)*100) 
    if((is.null(legend) == F) && (query_legend != tolower("none"))){
      if(query_legend == tolower("bottom")){
        legend_top <- 131
        legend_bottom <- 134
      }
      else if(query_legend == tolower("top")){
        bar_top <- 3
      matrix_bottom <- 102
      att_top <- 103
      att_bottom <- 132
      size_plot_height <- (size_plot_height + 2)
      legend_top <- 1
      legend_bottom <- 3
      }
      }
    grid.newpage()
    if((is.null(legend) == F) && (query_legend != tolower("none"))){
      if(query_legend == tolower("bottom")){
      pushViewport(viewport(layout = grid.layout(134, 100)))
      }
      else if(query_legend == tolower("top")){
        pushViewport(viewport(layout = grid.layout(132, 100)))
      }
    }
    else if((is.null(legend) == T) || (query_legend == tolower("none"))){
      pushViewport(viewport(layout = grid.layout(130, 100)))
    }
    print(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios), vp = vplayout(bar_top:matrix_bottom, 21:100))
    print(arrangeGrob(Size_plot), vp = vplayout(size_plot_height:matrix_bottom, 1:20))
    print(arrangeGrob(att_plot), vp = vplayout(att_top:att_bottom, 21:100))
    if((is.null(legend) == F) && (query_legend != tolower("none"))){
      print(arrangeGrob(legend), vp = vplayout(legend_top:legend_bottom, 21:100))
    }
  }
  else if(position == tolower("top")){
    bar_top <- 41
    matrix_bottom <- 140
    att_top <- 11
    att_bottom <- 40
    size_plot_height <- ((((hratios[1])+0.01)*100) + 40) 
    if((is.null(legend)) == F && (query_legend != tolower("none"))){
      if(query_legend == tolower("bottom")){
        legend_top <- 141
        legend_bottom <- 145
      }
      else if(query_legend == tolower("top")){
        bar_top <- 46
        matrix_bottom <- 145
        att_top <- 16
        att_bottom <- 45
        size_plot_height <- (size_plot_height + 5)
        legend_top <- 1
        legend_bottom <- 5
      }
    }
    grid.newpage()
    if((is.null(legend) == F) && (query_legend != tolower("none"))){
      if(query_legend == tolower("bottom")){
      pushViewport(viewport(layout = grid.layout(145, 100)))
      }
      else if(query_legend == tolower("top")){
        pushViewport(viewport(layout = grid.layout(150, 100)))
      }
    }
    else if(is.null(legend) == T || (query_legend == tolower("none"))){
      pushViewport(viewport(layout = grid.layout(140, 100)))
    }
    print(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios), vp = vplayout(bar_top:matrix_bottom, 21:100))
    print(arrangeGrob(Size_plot), vp = vplayout(size_plot_height:matrix_bottom, 1:20))
    print(arrangeGrob(att_plot), vp = vplayout(att_top:att_bottom, 21:100))
    if((is.null(legend) == F) && (query_legend != tolower("none"))){
      print(arrangeGrob(legend), vp = vplayout(legend_top:legend_bottom, 21:100))
    }
  }
}

ScatterAttPlot <- function(att_x, att_y, Set_data, start_col, labels, exp, elems_att, q_att, att_color,
                           Q_Title, customQ, hratios, position, size_plot_height, legend,
                           Main_bar_plot, Matrix_plot, Size_plot, query_legend){
  col_switch1 <- match(att_x, colnames(Set_data))
  col_switch2 <- match(att_y, colnames(Set_data))
  end_col <- ((start_col + as.integer(length(labels))) - 1)
  Set_data <- Set_data[which(rowSums(Set_data[ ,start_col:end_col]) != 0), ]
#  if(is.null(exp) == F){
#    Set_data <- Subset_att(Set_data, exp)
#  }
  colnames(Set_data)[col_switch1] <- "values1"
  colnames(Set_data)[col_switch2] <- "values2"
  if(is.null(elems_att) == F){
    elems <- elems_att
    if(nrow(elems) != 0){
      elems <- elems[order(elems$val1, elems$val2), ]
      EColors <- unique(elems$color)
    }
    else{
      elems <- NULL
    }
  }
  else{
    elems <- NULL
  }
  if(is.null(q_att) == F){
    intersect <- q_att
    if(nrow(intersect) != 0){
      intersect <- intersect[order(intersect$v1, intersect$v2), ]
    }
    else{
      intersect <- NULL
    }
  }
  else{
    intersect <- NULL
  }
  att_plot <- (ggplot(data = Set_data, aes(x = values1, y = values2)) 
               + geom_point(colour = att_color)
               + xlab(att_x) + ylab(att_y) + labs(title = Q_Title)
               + theme(panel.background = element_rect(fill = "white"),
                       plot.title = element_text(vjust = 1.3),
                       panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       axis.title.y = element_text(vjust = -0.8),
                       plot.margin=unit(c(-0.7,0.2,0.1,0.2), "cm")))
  if(is.null(elems) == F){
    att_plot <- att_plot + geom_point(data = elems, aes(x = val1, y = val2), colour = elems$color)
  }
  if(is.null(intersect) == F){
    att_plot <- (att_plot + geom_point(data = intersect, aes(x = v1, y = v2), color = intersect$color))
  }
  if(length(customQ) != 0){
    col1 <- match(att_x, colnames(customQ))
    col2 <- match(att_y, colnames(customQ))
    colnames(customQ)[col1] <- "cval1"
    colnames(customQ)[col2] <- "cval2"
    att_plot <- att_plot + geom_point(data = customQ, aes(x=cval1, y = cval2), color = customQ$color2)
  }
  
  att_plot <- ggplot_gtable(ggplot_build(att_plot))
  att_plot$widths <-  Matrix_plot$widths
  if((hratios[1] < 0.4) || 
       (hratios[2] > 0.6)) warning("Plot might be out of range if mb.ratio[1] < 0.4 or mb.ratio[2] >  0.6")
  if((is.null(position) == T) || (position == tolower("bottom"))){
    bar_top <- 1
    matrix_bottom <- 100
    att_top <- 101
    att_bottom <- 130
    size_plot_height <- (((hratios[1])+0.01)*100) 
    if((is.null(legend) == F) && (query_legend != tolower("none"))){
      if(query_legend == tolower("bottom")){
        legend_top <- 131
        legend_bottom <- 134
      }
      else if(query_legend == tolower("top")){
        bar_top <- 3
        matrix_bottom <- 102
        att_top <- 103
        att_bottom <- 132
        size_plot_height <- (size_plot_height + 2)
        legend_top <- 1
        legend_bottom <- 3
      }
    }
    grid.newpage()
    if((is.null(legend) == F) && (query_legend != tolower("none"))){
      if(query_legend == tolower("bottom")){
      pushViewport(viewport(layout = grid.layout(134, 100)))
      }
      else if(query_legend == tolower("top")){
        pushViewport(viewport(layout = grid.layout(132, 100)))
      }
    }
    else if((is.null(legend) == T) || (query_legend == tolower("none"))){
      pushViewport(viewport(layout = grid.layout(130, 100)))
    }
    print(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios), vp = vplayout(bar_top:matrix_bottom, 21:100))
    print(arrangeGrob(Size_plot), vp = vplayout(size_plot_height:matrix_bottom, 1:20))
    print(arrangeGrob(att_plot), vp = vplayout(att_top:att_bottom, 21:100))
    if((is.null(legend) == F) && (query_legend != tolower("none"))){
      print(arrangeGrob(legend), vp = vplayout(legend_top:legend_bottom, 21:100))
    }
  }
  else if(position == tolower("top")){
    bar_top <- 41
    matrix_bottom <- 140
    att_top <- 11
    att_bottom <- 40
    size_plot_height <- ((((hratios[1])+0.01)*100) + 40) 
    if((is.null(legend)) == F && (query_legend != tolower("none"))){
      if(query_legend == tolower("bottom")){
        legend_top <- 141
        legend_bottom <- 145
      }
      else if(query_legend == tolower("top")){
        bar_top <- 46
        matrix_bottom <- 145
        att_top <- 16
        att_bottom <- 45
        size_plot_height <- (size_plot_height + 5)
        legend_top <- 1
        legend_bottom <- 5
      }
    }
    grid.newpage()
    if((is.null(legend) == F) && (query_legend != tolower("none"))){
      if(query_legend == tolower("bottom")){
        pushViewport(viewport(layout = grid.layout(145, 100)))
      }
      else if(query_legend == tolower("top")){
        pushViewport(viewport(layout = grid.layout(150, 100)))
      }
    }
    else if(is.null(legend) == T || (query_legend == tolower("none"))){
      pushViewport(viewport(layout = grid.layout(140, 100)))
    }
    print(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios), vp = vplayout(bar_top:matrix_bottom, 21:100))
    print(arrangeGrob(Size_plot), vp = vplayout(size_plot_height:matrix_bottom, 1:20))
    print(arrangeGrob(att_plot), vp = vplayout(att_top:att_bottom, 21:100))
    if((is.null(legend) == F) && (query_legend != tolower("none"))){
      print(arrangeGrob(legend), vp = vplayout(legend_top:legend_bottom, 21:100))
    }
  }
}

BaseBoxPlot <- function(custom_plot, position, size_plot_height, Main_bar_plot, Matrix_plot, 
                           Size_plot, hratios, query_legend){
  bar_top <- 1
  matrix_bottom <- 100
  att_top <- 101
  att_bottom <- 130
  if((is.null(position) == F) && (position != tolower("bottom"))){
    size_plot_height <- (size_plot_height + 30)
    bar_top <- 31
    matrix_bottom <- 130
    att_top <- 1
    att_bottom <- 30
  }
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(130,100)))
  print(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios), vp = vplayout(bar_top:matrix_bottom, 21:100))
  print(arrangeGrob(Size_plot), vp = vplayout(size_plot_height:matrix_bottom, 1:20))
  print(arrangeGrob(custom_plot), vp = vplayout(att_top:att_bottom, 21:100))
}

GenerateCustomPlots <- function(custom_plot, Set_data){
  CustomPlot <- list()
  for(i in 1:length(custom_plot$plots)){
    CustomPlot[[i]] <- custom_plot$plots[[i]]$plot(Set_data)
  }
  return(CustomPlot)
}

BaseCustomPlot <- function(plots, custom_plot, position, size_plot_height, Main_bar_plot, Matrix_plot, 
                           Size_plot, hratios, query_legend){
  bar_top <- 1
  matrix_bottom <- 100
  custom_top <- 101
  custom_bottom <- (custom_plot$nrows + 100)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(custom_bottom,100)))
  print(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios), vp = vplayout(bar_top:matrix_bottom, 21:100))
  print(arrangeGrob(Size_plot), vp = vplayout(size_plot_height:matrix_bottom, 1:20))
  for(i in 1:length(custom_plot$plots)){
    print(plots[[i]], vp = vplayout(custom_plot$plots[[i]]$rows, custom_plot$plots[[i]]$cols))
  }
}