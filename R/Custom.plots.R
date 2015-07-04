
## Generates list of custom plots to be plotted underneath UpSet plot
GenerateCustomPlots <- function(attribute_plots, Set_data, QueryData, att_color, attx, atty, names){
  CustomPlot <- list()
  Set_data$color <- att_color
  Set_data <- Set_data[ ,-which(names(Set_data) %in% names)]
  if(length(QueryData) != 0){
    SetAndQueryData <- data.frame(rbind(Set_data, QueryData))
  }
  for(i in seq_along(attribute_plots$plots)){
    if(length(QueryData) != 0){SetAndQueryData[1:nrow(Set_data), ]$color <- "gray23"}
    #      x_att <- attribute_plots$plots[[i]]$x
    #      y_att <- attribute_plots$plots[[i]]$y
    if(isTRUE(attribute_plots$plots[[i]]$queries) == T){
      if(length(QueryData) == 0){
        warning("To overlay with query data please specify att.x and att.y where applicable.")
        if(is.null(attribute_plots$plots[[i]]$y) == F){
          CustomPlot[[i]] <- attribute_plots$plots[[i]]$plot(Set_data, attribute_plots$plots[[i]]$x, attribute_plots$plots[[i]]$y)
        }
        else{
          CustomPlot[[i]] <- attribute_plots$plots[[i]]$plot(Set_data, attribute_plots$plots[[i]]$x)
        }
      }
      else if(length(QueryData) != 0){
        if(is.null(attribute_plots$plots[[i]]$y) == F){
          if(is.na(atty[i]) == T){
            warning("No y attribute provided to overlay with query data.
                        If attempting to display plot that needs both x and y aesthetics please enter att.y parameter.
                        Plots that require just the x aestheitc will not be affected.")
          }
          if(att_color == "gray23"){SetAndQueryData[1:nrow(Set_data), ]$color <- "gray65"}
          CustomPlot[[i]] <- attribute_plots$plots[[i]]$plot(SetAndQueryData, attribute_plots$plots[[i]]$x, attribute_plots$plots[[i]]$y)
        }
        else{
           if(att_color == "gray23"){SetAndQueryData[1:nrow(Set_data), ]$color <- "gray65"}
          CustomPlot[[i]] <- attribute_plots$plots[[i]]$plot(SetAndQueryData, attribute_plots$plots[[i]]$x)
        }
        
      }
    }
    else {
      if(is.null(attribute_plots$plots[[i]]$y) == F){
        CustomPlot[[i]] <- attribute_plots$plots[[i]]$plot(Set_data, attribute_plots$plots[[i]]$x, attribute_plots$plots[[i]]$y)
      }
      else{
        CustomPlot[[i]] <- attribute_plots$plots[[i]]$plot(Set_data, attribute_plots$plots[[i]]$x)
      }
    }
  }
  return(CustomPlot)
}
