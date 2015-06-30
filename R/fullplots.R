#Viewport function 
vplayout <- function(x,y){
  viewport(layout.pos.row = x, layout.pos.col = y)
}

#Generates UpSet plot when no attributes are selected to be plotted
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


#Generates UpSet plot when two attributes are selected to be plotted against each other
ScatterAttPlot <- function(att_x, att_y, Set_data, start_col, labels, exp, att_color, QueryData,
                           Q_Title, hratios, position, size_plot_height, legend,
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

  att_plot <- (ggplot(data = Set_data, aes(x = values1, y = values2)) 
               + geom_point(colour = att_color)
               + xlab(att_x) + ylab(att_y) + labs(title = Q_Title)
               + theme(panel.background = element_rect(fill = "white"),
                       plot.title = element_text(vjust = 1.3),
                       panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       axis.title.y = element_text(vjust = -0.8),
                       plot.margin=unit(c(-0.7,0.2,0.1,0.2), "cm")))

if(is.null(QueryData) == F){
  att_plot <- att_plot + geom_point(data = QueryData, aes(x=val1, y = val2), colour = QueryData$color)
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

#Generates UpSet plot with boxplots representing distributions of attributes
BaseBoxPlot <- function(box_plot, position, size_plot_height, Main_bar_plot, Matrix_plot, 
                           Size_plot, hratios){
  if(length(box_plot) > 2){
    warning("UpSet can only show 2 box plots at a time")
  }
  if(is.null(position) == T || position == tolower("bottom")){
  bar_top <- 1
  matrix_bottom <- 100
  att_top <- 101
  att_bottom <- 130
  if(length(box_plot) == 2){
    att_top <- 105
    att_bottom <- 120
    gridrow <- 145
  }
  }
  if((is.null(position) == F) && (position != tolower("bottom"))){
    if(length(box_plot)==1){
    size_plot_height <- (size_plot_height + 35)
    bar_top <- 36
    matrix_bottom <- 135
    att_top <- 10
    att_bottom <- 35
    }
    else if(length(box_plot) == 2){
      size_plot_height <- (size_plot_height + 50)
      bar_top <- 51
      matrix_bottom <- 150
      att_top <- 15 
      att_bottom <- 30
      gridrow <- 150
    }
  }
  grid.newpage()
  if(length(box_plot) == 1){
  pushViewport(viewport(layout = grid.layout(135,100)))
  }
  else if(length(box_plot) == 2){
    pushViewport(viewport(layout = grid.layout(gridrow,100)))
  }
  print(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios), vp = vplayout(bar_top:matrix_bottom, 21:100))
  print(arrangeGrob(Size_plot), vp = vplayout(size_plot_height:matrix_bottom, 1:20))
  print(arrangeGrob(box_plot[[1]]), vp = vplayout(att_top:att_bottom, 21:100))
  if(length(box_plot) == 2){
  print(arrangeGrob(box_plot[[2]]), vp = vplayout((att_bottom + 10):(att_bottom + 25), 21:100))
  }
}

#Generates list of custom plots to be plotted underneath UpSet plot
GenerateCustomPlots <- function(custom_plot, Set_data, QueryData, att_color, attx, atty){
  CustomPlot <- list()
  Set_data$color <- att_color
  if(length(QueryData) != 0){
    if(is.null(attx) == F && is.null(atty) == F){
  SetAndQueryData <- Set_data[c(attx, atty, "color")]
  colnames(QueryData) <- c(attx, atty, "color")
    }
  else if(is.null(attx) == F && is.null(atty) == T){
    SetAndQueryData <- Set_data[c(attx, "color")]
    colnames(QueryData) <- c(attx, "color")
  }
  SetAndQueryData <- data.frame(rbind(SetAndQueryData, QueryData))
  SetAndQueryData <- SetAndQueryData[order(SetAndQueryData[1]), ]
  }
  for(i in seq_along(custom_plot$plots)){
#      x_att <- custom_plot$plots[[i]]$x
#      y_att <- custom_plot$plots[[i]]$y
    if(isTRUE(custom_plot$plots[[i]]$queries) == T){ 
      if(length(QueryData) == 0){
        warning("To overlay with query data please specify att.x and att.y where applicable.")
        if(is.null(custom_plot$plots[[i]]$y) == F){
        CustomPlot[[i]] <- custom_plot$plots[[i]]$plot(Set_data, custom_plot$plots[[i]]$x, custom_plot$plots[[i]]$y)
        }
        else{
          CustomPlot[[i]] <- custom_plot$plots[[i]]$plot(Set_data, custom_plot$plots[[i]]$x)
        }
      }
      else if(length(QueryData) != 0){
        if(is.null(custom_plot$plots[[i]]$y) == F){
          if(is.null(atty) == T){
            warning("No y attribute provided to overlay with query data.
      If attempting to display plot that needs both x and y aesthetics please enter att.y parameter.
      Plots that require just the x aestheitc will not be affected.")
          }
     CustomPlot[[i]] <- custom_plot$plots[[i]]$plot(SetAndQueryData, custom_plot$plots[[i]]$x, custom_plot$plots[[i]]$y)
        }
     else{
       CustomPlot[[i]] <- custom_plot$plots[[i]]$plot(SetAndQueryData, custom_plot$plots[[i]]$x)
     }
     
      }
    }
    else {
      if(is.null(custom_plot$plots[[i]]$y) == F){
      CustomPlot[[i]] <- custom_plot$plots[[i]]$plot(Set_data, custom_plot$plots[[i]]$x, custom_plot$plots[[i]]$y)
      }
      else{
        CustomPlot[[i]] <- custom_plot$plots[[i]]$plot(Set_data, custom_plot$plots[[i]]$x)
      }
    }
  }
  return(CustomPlot)
}

#Function that plots out the list of plots generated from custom plot input
BaseCustomPlot <- function(custom_plot, plots, position, size_plot_height, Main_bar_plot, Matrix_plot, 
                           Size_plot, hratios){
  bar_top <- 1
  matrix_bottom <- 100
  custom_top <- 101
  custom_bottom <- (custom_plot$nrows + 100)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(custom_bottom,100)))
  print(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios), vp = vplayout(bar_top:matrix_bottom, 21:100))
  print(arrangeGrob(Size_plot), vp = vplayout(size_plot_height:matrix_bottom, 1:20))
    print(do.call(arrangeGrob, c(plots, ncol = custom_plot$ncols)),
                  vp = vplayout(custom_top:custom_bottom, 1:100), newpage = F)
#   print(custom_plot$plot, vp = vplayout(custom_plot$rows, custom_plot$cols), newpage = F)
}
# printCustom <- function(custom_plot){
#   print(custom_plot$plot, vp = vplayout(custom_plot$rows, custom_plot$cols), newpage = F)
# }