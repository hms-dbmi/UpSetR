
## Generates list of custom plots to be plotted underneath UpSet plot
GenerateCustomPlots <- function(custom_plot, Set_data, QueryData, att_color, attx, atty, names){
  CustomPlot <- list()
  Set_data$color <- att_color
  Set_data <- Set_data[ ,-which(names(Set_data) %in% names)]
  if(length(QueryData) != 0){
    SetAndQueryData <- data.frame(rbind(Set_data, QueryData))
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
          if(is.na(atty[i]) == T){
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
