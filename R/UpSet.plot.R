#' @importFrom grid grid.newpage
#' @importFrom grid pushViewport
#' @importFrom grid viewport
#' @importFrom grid grid.layout
#' @importFrom grid grid.draw
#' @importFrom grid popViewport
#' @importFrom grid legendGrob
#' @importFrom grid gpar

## Assemble plots to make UpSet plot
Make_base_plot <- function(Main_bar_plot, Matrix_plot, Size_plot, labels, hratios, att_x, att_y,
                           Set_data, exp, position, start_col, att_color, QueryData,
                           attribute_plots, legend, query_legend, boxplot, names, set_metadata,
                           set_metadata_plots, newpage){

  end_col <- ((start_col + as.integer(length(labels))) - 1)
  Set_data <- Set_data[which(rowSums(Set_data[ ,start_col:end_col]) != 0), ]
  Main_bar_plot$widths <- Matrix_plot$widths
  Matrix_plot$heights <- Size_plot$heights
  if(!is.null(set_metadata)){
    ncols <- 0
    for(i in 1:length(set_metadata_plots)){
      ncols <- ncols + set_metadata$plots[[i]]$assign
      set_metadata_plots[[i]]$heights <- Size_plot$heights
    }
    set_metadata$ncols <- ncols
  }
  if(!is.null(legend)) {
    legend$widths <- Matrix_plot$widths
  }
  if(!is.null(boxplot)){
    for(i in seq_along(boxplot)){
      boxplot[[i]]$widths <- Matrix_plot$widths
    }
  }

  size_plot_height <- (((hratios[1])+0.01)*100)
  if((hratios[1] > 0.7 || hratios[1] < 0.3) ||
     (hratios[2] > 0.7 || hratios[2] < 0.3)) warning("Plot might be out of range if ratio > 0.7 or < 0.3")
  if(is.null(attribute_plots) && is.null(boxplot)){
    NoAttBasePlot(legend, size_plot_height, Main_bar_plot, Matrix_plot, hratios, Size_plot, query_legend,
                  set_metadata, set_metadata_plots, newpage=newpage)
  }
  else if(!is.null(attribute_plots) && is.null(boxplot)){
    plots <- GenerateCustomPlots(attribute_plots, Set_data, QueryData, att_color, att_x, att_y, names)
    #      for(i in seq_along(plots)){
    #        attribute_plots$plots[[i]]$plot <- plots[[i]]
    #      }
    BaseCustomPlot(attribute_plots, plots, position, size_plot_height, Main_bar_plot, Matrix_plot, Size_plot,
                   hratios, legend, query_legend, set_metadata, set_metadata_plots, newpage=newpage)
  }
  else if(!is.null(boxplot) && is.null(attribute_plots)){
    BaseBoxPlot(boxplot, position, size_plot_height, Main_bar_plot, Matrix_plot, Size_plot,
                hratios, set_metadata, set_metadata_plots, newpage=newpage)
  }
}

## Viewport function
vplayout <- function(x,y){
  viewport(layout.pos.row = x, layout.pos.col = y)
}

## Generates UpSet plot with boxplots representing distributions of attributes
BaseBoxPlot <- function(box_plot, position, size_plot_height, Main_bar_plot, Matrix_plot,
                        Size_plot, hratios, set_metadata, set_metadata_plots, newpage){

  if(length(box_plot) > 2){
    return(warning("UpSet can only show 2 box plots at a time"))
  }
  if(is.null(position) || position == tolower("bottom")){
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
  if((!is.null(position)) && (position != tolower("bottom"))){
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
  if(is.null(set_metadata)){
    matrix_and_mainbar_right <- 100
    matrix_and_mainbar_left <- 21
    size_bar_right <- 20
    size_bar_left <- 1
  }
  else if(!is.null(set_metadata)){
    matrix_and_mainbar_right <- set_metadata$ncols + 100
    matrix_and_mainbar_left <- set_metadata$ncols + 21
    size_bar_right <- set_metadata$ncols + 20
    size_bar_left <- set_metadata$ncols + 1
    metadata_right <- set_metadata$ncols
    metadata_left <- 1
  }
  if (newpage) {
    grid.newpage()
  }
  if(length(box_plot) == 1){
    pushViewport(viewport(layout = grid.layout(135,matrix_and_mainbar_right)))
  }
  else if(length(box_plot) == 2){
    pushViewport(viewport(layout = grid.layout(gridrow,matrix_and_mainbar_right)))
  }
  vp = vplayout(bar_top:matrix_bottom, matrix_and_mainbar_left:matrix_and_mainbar_right)
  pushViewport(vp)
  grid.draw(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios))
  popViewport()
  vp = vplayout(size_plot_height:matrix_bottom, size_bar_left:size_bar_right)
  pushViewport(vp)
  grid.draw(arrangeGrob(Size_plot))
  popViewport()
  if(!is.null(set_metadata)){
    for(i in 1:length(set_metadata_plots)){
      if(i != 1){
        metadata_left <- 1+metadata_right
        metadata_right <- metadata_right + set_metadata$plots[[i]]$assign
      }
      else{
        metadata_left <- 1
        metadata_right <- set_metadata$plots[[i]]$assign
      }

      vp = vplayout(size_plot_height:matrix_bottom, metadata_left:metadata_right)
      pushViewport(vp)
      grid.draw(arrangeGrob(set_metadata_plots[[i]]))
      popViewport()
    }
  }
  vp = vplayout(att_top:att_bottom, matrix_and_mainbar_left:matrix_and_mainbar_right)
  pushViewport(vp)
  grid.draw(arrangeGrob(box_plot[[1]]))
  popViewport()
  if(length(box_plot) == 2){
    vp = vplayout((att_bottom + 10):(att_bottom + 25), matrix_and_mainbar_left:matrix_and_mainbar_right)
    pushViewport(vp)
    grid.draw(arrangeGrob(box_plot[[2]]))
    popViewport()
  }
}

## Generates UpSet plot when no attributes are selected to be plotted
NoAttBasePlot <- function(legend, size_plot_height, Main_bar_plot, Matrix_plot, hratios,
                          Size_plot, query_legend, set_metadata, set_metadata_plots, newpage){
  top <- 1
  bottom <- 100
  if((!is.null(legend)) && (query_legend != tolower("none"))){
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
  if(is.null(set_metadata)){
    matrix_and_mainbar_right <- 100
    matrix_and_mainbar_left <- 21
    size_bar_right <- 20
    size_bar_left <- 1
  }
  else if(!is.null(set_metadata)){
    matrix_and_mainbar_right <- set_metadata$ncols + 100
    matrix_and_mainbar_left <- set_metadata$ncols + 21
    size_bar_right <- set_metadata$ncols + 20
    size_bar_left <- set_metadata$ncols + 1
    metadata_right <- set_metadata$ncols
    metadata_left <- 1
  }
  if (newpage) {
    grid.newpage()
  }
  if((!is.null(legend)) && (query_legend != tolower("none"))){
    if(query_legend == tolower("top")){
      pushViewport(viewport(layout = grid.layout(102, matrix_and_mainbar_right)))
    }
    else if(query_legend == tolower("bottom")){
      pushViewport(viewport(layout = grid.layout(103, matrix_and_mainbar_right)))
    }
  }
  else if((is.null(legend))|| (query_legend == tolower("none"))){
    pushViewport(viewport(layout = grid.layout(100,matrix_and_mainbar_right)))
  }
  vp = vplayout(top:bottom, matrix_and_mainbar_left:matrix_and_mainbar_right)
  pushViewport(vp)
  grid.draw(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios))
  popViewport()
  vp = vplayout(size_plot_height:bottom, size_bar_left:size_bar_right)
  pushViewport(vp)
  grid.draw(arrangeGrob(Size_plot))
  popViewport()
  if(!is.null(set_metadata)){
    for(i in 1:length(set_metadata_plots)){
      if(i != 1){
        metadata_left <- 1+metadata_right
        metadata_right <- metadata_right + set_metadata$plots[[i]]$assign
      }
      else{
        metadata_left <- 1
        metadata_right <- set_metadata$plots[[i]]$assign
      }

      vp = vplayout(size_plot_height:bottom, metadata_left:metadata_right)
      pushViewport(vp)
      grid.draw(arrangeGrob(set_metadata_plots[[i]]))
      popViewport()
    }
  }
  if((!is.null(legend)) && (query_legend != tolower("none"))){
    vp = vplayout(legend_top:legend_bottom, matrix_and_mainbar_left:matrix_and_mainbar_right)
    pushViewport(vp)
    grid.draw(arrangeGrob(legend))
    popViewport()
  }
}

## Function that plots out the list of plots generated from custom plot input
BaseCustomPlot <- function(attribute_plots, plots, position, size_plot_height, Main_bar_plot, Matrix_plot,
                           Size_plot, hratios, legend, q_legend, set_metadata, set_metadata_plots, newpage){
  bar_top <- 1
  matrix_bottom <- 100
  custom_top <- 101
  custom_bottom <- (attribute_plots$gridrows + 100)

  if(is.null(set_metadata)){
    matrix_and_mainbar_right <- 100
    matrix_and_mainbar_left <- 21
    size_bar_right <- 20
    size_bar_left <- 1
  }
  else if(!is.null(set_metadata)){
    matrix_and_mainbar_right <- set_metadata$ncols + 100
    matrix_and_mainbar_left <- set_metadata$ncols + 21
    size_bar_right <- set_metadata$ncols + 20
    size_bar_left <- set_metadata$ncols + 1
    metadata_right <- set_metadata$ncols
    metadata_left <- 1
  }

  if((!is.null(legend)) && (q_legend == tolower("bottom"))){custom_bottom <- (custom_bottom + 5)}
  if((!is.null(legend)) && (q_legend == tolower("top"))){
    bar_top <- bar_top + 5
    matrix_bottom <- matrix_bottom + 5
    custom_top <- custom_top + 5
    custom_bottom <- custom_bottom + 5
  }
  if (newpage) {
    grid.newpage()
  }
  pushViewport(viewport(layout = grid.layout(custom_bottom,matrix_and_mainbar_right)))
  vp = vplayout(bar_top:matrix_bottom, matrix_and_mainbar_left:matrix_and_mainbar_right)
  pushViewport(vp)
  grid.draw(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios))
  popViewport()
  vp = vplayout(size_plot_height:matrix_bottom, size_bar_left:size_bar_right)
  pushViewport(vp)
  grid.draw(arrangeGrob(Size_plot))
  popViewport()
  if(!is.null(set_metadata)){
    for(i in 1:length(set_metadata_plots)){
      if(i != 1){
        metadata_left <- 1+metadata_right
        metadata_right <- metadata_right + set_metadata$plots[[i]]$assign
      }
      else{
        metadata_left <- 1
        metadata_right <- set_metadata$plots[[i]]$assign
      }

      vp = vplayout(size_plot_height:matrix_bottom, metadata_left:metadata_right)
      pushViewport(vp)
      grid.draw(arrangeGrob(set_metadata_plots[[i]]))
      popViewport()
    }
  }
  if((!is.null(legend)) && (q_legend == tolower("bottom"))){
    vp = vplayout(custom_top:(custom_bottom - 5), 1:matrix_and_mainbar_right)
    pushViewport(vp)
    grid.draw(do.call(arrangeGrob, c(plots, ncol = attribute_plots$ncols)))
    popViewport()
    vp = vplayout((custom_bottom - 4):custom_bottom, 1:matrix_and_mainbar_right)
    pushViewport(vp)
    grid.draw(arrangeGrob(legend))
    popViewport()
  }
  else if((!is.null(legend)) && (q_legend == tolower("top"))){
    vp = vplayout(custom_top:custom_bottom, 1:matrix_and_mainbar_right)
    pushViewport(vp)
    grid.draw(do.call(arrangeGrob, c(plots, ncol = attribute_plots$ncols)))
    popViewport()
    vp = vplayout((bar_top-5):(bar_top-1), 1:matrix_and_mainbar_right)
    pushViewport(vp)
    grid.draw(arrangeGrob(legend))
    popViewport()
  }
  else{
    vp = vplayout(custom_top:custom_bottom, 1:matrix_and_mainbar_right)
    pushViewport(vp)
    grid.draw(do.call(arrangeGrob, c(plots, ncol = attribute_plots$ncols)))
    popViewport()
  }
  #   print(attribute_plots$plot, vp = vplayout(attribute_plots$rows, attribute_plots$cols), newpage = F)
}

# printCustom <- function(attribute_plots){
#   print(attribute_plots$plot, vp = vplayout(attribute_plots$rows, attribute_plots$cols), newpage = F)
# }
