## Assemble plots to make UpSet plot
Make_base_plot <- function(Main_bar_plot, Matrix_plot, Size_plot, labels, hratios, att_x, att_y,
                           Set_data, exp, position, start_col, att_color, QueryData,
                           attribute_plots, legend, query_legend, boxplot, names){
  
  end_col <- ((start_col + as.integer(length(labels))) - 1)
  Set_data <- Set_data[which(rowSums(Set_data[ ,start_col:end_col]) != 0), ]
  Main_bar_plot$widths <- Matrix_plot$widths
  Matrix_plot$heights <- Size_plot$heights
  if(is.null(legend)==F){
    legend$widths <- Matrix_plot$widths
  }
  if(is.null(boxplot) == F){
    for(i in seq_along(boxplot)){
      boxplot[[i]]$widths <- Matrix_plot$widths
    }
  }
  
  size_plot_height <- (((hratios[1])+0.01)*100)
  if((hratios[1] > 0.7 || hratios[1] < 0.3) ||
       (hratios[2] > 0.7 || hratios[2] < 0.3)) warning("Plot might be out of range if ratio > 0.7 or < 0.3")
  if(is.null(attribute_plots) == T && is.null(boxplot) == T){
      NoAttBasePlot(legend, size_plot_height, Main_bar_plot, Matrix_plot, hratios, Size_plot, query_legend)
  }
  else if(is.null(attribute_plots) == F && is.null(boxplot) == T){
    plots <- GenerateCustomPlots(attribute_plots, Set_data, QueryData, att_color, att_x, att_y, names)
    #      for(i in seq_along(plots)){
    #        attribute_plots$plots[[i]]$plot <- plots[[i]]
    #      }
    BaseCustomPlot(attribute_plots, plots, position, size_plot_height, Main_bar_plot, Matrix_plot, Size_plot,
                   hratios, legend, query_legend)
  }
  else if(is.null(boxplot)==F && is.null(attribute_plots) == T){
    BaseBoxPlot(boxplot, position, size_plot_height, Main_bar_plot, Matrix_plot, Size_plot,
                hratios)
  }
}

## Viewport function
vplayout <- function(x,y){
  viewport(layout.pos.row = x, layout.pos.col = y)
}

## Generates UpSet plot with boxplots representing distributions of attributes
BaseBoxPlot <- function(box_plot, position, size_plot_height, Main_bar_plot, Matrix_plot,
                        Size_plot, hratios){
  if(length(box_plot) > 2){
    return(warning("UpSet can only show 2 box plots at a time"))
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

## Generates UpSet plot when no attributes are selected to be plotted
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

## Function that plots out the list of plots generated from custom plot input
BaseCustomPlot <- function(attribute_plots, plots, position, size_plot_height, Main_bar_plot, Matrix_plot,
                           Size_plot, hratios, legend, q_legend){
  bar_top <- 1
  matrix_bottom <- 100
  custom_top <- 101
  custom_bottom <- (attribute_plots$gridrows + 100)
  if((is.null(legend) == F) && (q_legend != tolower("none"))){custom_bottom <- (custom_bottom + 5)}
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(custom_bottom,100)))
  print(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios), vp = vplayout(bar_top:matrix_bottom, 21:100))
  print(arrangeGrob(Size_plot), vp = vplayout(size_plot_height:matrix_bottom, 1:20))
  if((is.null(legend) == F) && (q_legend != tolower("none"))){
    print(do.call(arrangeGrob, c(plots, ncol = attribute_plots$ncols)),
          vp = vplayout(custom_top:(custom_bottom - 5), 1:100), newpage = F)
    print(arrangeGrob(legend), vp = vplayout((custom_bottom - 4):custom_bottom, 1:100))
  }
  else{
    print(do.call(arrangeGrob, c(plots, ncol = attribute_plots$ncols)),
          vp = vplayout(custom_top:custom_bottom, 1:100), newpage = F)
  }
  #   print(attribute_plots$plot, vp = vplayout(attribute_plots$rows, attribute_plots$cols), newpage = F)
}
# printCustom <- function(attribute_plots){
#   print(attribute_plots$plot, vp = vplayout(attribute_plots$rows, attribute_plots$cols), newpage = F)
# }
