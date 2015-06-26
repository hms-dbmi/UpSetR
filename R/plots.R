Make_main_bar <- function(Main_bar_data, Q, show_num, ratios, customQ){
  if(is.null(Q) == F){
    inter_data <- Q
    if(nrow(inter_data) != 0){
      inter_data <- inter_data[order(inter_data$x), ]
    }
    else{
      inter_data <- NULL
    }
  }
  else{
    inter_data <- NULL
  }
  ten_perc <- ((max(Main_bar_data$freq)) * 0.1)
  Main_bar_plot <- (ggplot(data = Main_bar_data, aes(x = x, y = freq)) 
                    + geom_bar(stat = "identity", colour = Main_bar_data$color, width = 0.6, 
                               fill = Main_bar_data$color)
                    + scale_x_continuous(limits = c(0,(nrow(Main_bar_data)+1 )), expand = c(0,0),
                                         breaks = NULL)
                    + scale_y_continuous(limits = c(0, max(Main_bar_data$freq) + ten_perc), 
                                         expand = c(c(0,0), c(0,0)))
                    + xlab(NULL) + ylab("Intersection Size") +labs(title = NULL)
                    + theme(panel.background = element_rect(fill = "white"),
                            plot.margin = unit(c(0.5,0.5,0.1,0.5), "lines"), panel.border = element_blank(),
                            axis.title.y = element_text(vjust = -0.8)))
  if((show_num == "yes") || (show_num == "Yes")){
    Main_bar_plot <- (Main_bar_plot + geom_text(aes(label = freq), size = 3.0, vjust = -0.4, colour = Main_bar_data$color))
  }
  bInterDat <- NULL
  pInterDat <- NULL
  bCustomDat <- NULL
  pCustomDat <- NULL
  if(is.null(inter_data) == F){
    bInterDat <- inter_data[which(inter_data$act == T), ]
    bInterDat <- bInterDat[order(bInterDat$x), ]
    pInterDat <- inter_data[which(inter_data$act == F), ]
  }
  if(length(customQ) != 0){
    pCustomDat <- customQ[which(customQ$act == F), ]
    bCustomDat <- customQ[which(customQ$act == T), ]
    bCustomDat <- bCustomDat[order(bCustomDat$x), ]
  }
    if(length(bInterDat) != 0){
    Main_bar_plot <- Main_bar_plot + geom_bar(data = bInterDat,
                                              aes(x=x, y = freq), colour = bInterDat$color,
                                              fill = bInterDat$color, colour ="black",
                                              stat = "identity", position = "identity", width = 0.6)
    }
  if(length(bCustomDat) != 0){
    
    Main_bar_plot <- (Main_bar_plot + geom_bar(data = bCustomDat, aes(x=x, y = freq2),
                                               fill = bCustomDat$color2, colour = "black",
                                               stat = "identity", position ="identity", width = 0.6))
  }
  if(length(pCustomDat) != 0){
  Main_bar_plot <- (Main_bar_plot + geom_point(data = pCustomDat, aes(x=x, y = freq2), colour = pCustomDat$color2,
                                               size = 2, shape = 17, position = position_jitter(w = 0.2, h = 0.2)))
  }
  if(length(pInterDat) != 0){
    Main_bar_plot <- (Main_bar_plot + geom_point(data = pInterDat, aes(x=x, y = freq),
                                                 position = position_jitter(w = 0.2, h = 0.2),
                                                 colour = pInterDat$color, size = 2, shape = 17))
  }
  
  Main_bar_plot <- (Main_bar_plot 
                    + geom_vline(xintercept = 0, color = "gray0")
                    + geom_hline(yintercept = 0, color = "gray0"))
  
  Main_bar_plot <- ggplotGrob(Main_bar_plot)
  return(Main_bar_plot)
}

MakeShading <- function(Mat_data){
  y <- unique(Mat_data$y)
  y <- (y[which(y %% 2 != 0)])
  data <- data.frame(cbind(y))
  data$min <- 0
  data$max <- (max(Mat_data$x) + 1)
  for( i in 1:length(y)){
    data$y_min[i] <- ((y[i]) - 0.5)
    data$y_max[i] <- ((y[i]) + 0.5)
  }
  return(data)
}

Make_matrix_plot <- function(Mat_data,Set_size_data, Main_bar_data, point_size, line_size, name_size, labels,
                             shading_data, shade_color, shade_alpha){
  Matrix_plot <- (ggplot() 
                  + theme(panel.background = element_rect(fill = "white"),
                          plot.margin=unit(c(-0.2,0.5,0.5,0.5), "lines"),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank(),
                          axis.ticks.y = element_blank(),
                          axis.text.y = element_text(colour = "gray0", 
                                                     size = name_size, hjust = 0.4),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank())
                  + xlab(NULL) + ylab("   ")
                  + scale_y_continuous(breaks = c(1:nrow(Set_size_data)),
                                       limits = c(0.5,(nrow(Set_size_data) +0.5)),
                                       labels = labels, expand = c(0,0))
                  + scale_x_continuous(limits = c(0,(nrow(Main_bar_data)+1 )), expand = c(0,0))
                  + geom_rect(data = shading_data, aes(xmin = min, xmax = max, ymin = y_min, ymax = y_max ),
                              fill = shade_color, alpha = shade_alpha)
                  + geom_point(data=Mat_data, aes(x= x, y= y), colour = Mat_data$color, size= point_size)
                  + geom_line(data = Mat_data, aes(group = Intersection, x=x, y=y), 
                              size = line_size, colour = Mat_data$color))
  Matrix_plot <- ggplot_gtable(ggplot_build(Matrix_plot))
  return(Matrix_plot)
}

Make_size_plot <- function(Set_size_data, sbar_color, ratios){
  if(ratios[2] < 0.46){
    m <- 0.4
  }
  else if((ratios[2] > 0.45) & (ratios[2] < 0.66)){
    m <- 0.35
  }
  else{
    m <- 0.3
  }
  Size_plot <- (ggplot(data = Set_size_data, aes(x =x, y = y))
                + geom_bar(stat = "identity",colour = sbar_color, width = 0.4,
                           fill = sbar_color, position = "identity")
                + scale_x_continuous(limits = c(0.5, (nrow(Set_size_data)+0.5)),
                                     breaks = c(0, max(Set_size_data)),
                                     expand = c(0,0))
                + theme(panel.background = element_rect(fill = "white"),
                        plot.margin=unit(c(-m,-1.3,0.5,0.5), "lines"),
                        axis.title.x = element_text(size = 10, face = "bold"),
                        axis.line = element_line(colour = "gray0"),
                        axis.line.y = element_line(colour = "white"),
                        axis.text.y = element_blank(),
                        axis.ticks.y = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank())
                + xlab(NULL) + ylab("Set Size")
                + coord_flip()
                + scale_y_reverse())
  
  Size_plot <- ggplot_gtable(ggplot_build(Size_plot))
  return(Size_plot)
}

Make_base_plot <- function(Main_bar_plot, Matrix_plot, Size_plot, labels, hratios, att_x, att_y,
                           Set_data, exp, position, start_col, att_color, QueryData,
                           Q_Title, custom_plot, legend, query_legend, boxplot){
  
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
  if(is.null(custom_plot) == T && is.null(boxplot) == T){
    if((is.null(att_x) == T) && (is.null(att_y) == F)){
      warning("Please place lone attribute in att.x if you wish to plot a histogram")
    }
    
    else if((is.null(att_x) == T) && (is.null(att_y) == T)){
      NoAttBasePlot(legend, size_plot_height, Main_bar_plot, Matrix_plot, hratios, Size_plot, query_legend)
    }
    
    else if((is.null(att_x) == F) && (is.null(att_y) == T)){
      HistoAttPlot(att_x, att_y, Set_data, start_col, labels, exp, att_color, QueryData,
                   Q_Title, hratios, position, size_plot_height, legend,
                   Main_bar_plot, Matrix_plot, Size_plot, query_legend)
    }
    
    else if((is.null(att_x) == F) && (is.null(att_y) == F)){
      ScatterAttPlot(att_x, att_y, Set_data, start_col, labels, exp, att_color, QueryData,
                     Q_Title, hratios, position, size_plot_height, legend,
                     Main_bar_plot, Matrix_plot, Size_plot, query_legend)
    }
  }
  
  else if(is.null(custom_plot) == F && is.null(boxplot) == T){
   plots <- GenerateCustomPlots(custom_plot, Set_data, QueryData, att_color, att_x, att_y)
#      for(i in seq_along(plots)){
#        custom_plot$plots[[i]]$plot <- plots[[i]]
#      }
         BaseCustomPlot(custom_plot, plots, position, size_plot_height, Main_bar_plot, Matrix_plot, Size_plot,
                        hratios)
  }
  else if(is.null(boxplot)==F && is.null(custom_plot) == T){
    BaseBoxPlot(boxplot, position, size_plot_height, Main_bar_plot, Matrix_plot, Size_plot,
                hratios)
  }
}

IntersectionBoxPlot <- function(data1, data2, start_col, names){
  end_col <- ((start_col + length(names)) - 1)
  data2 <- data2[which(rowSums(data2[ ,start_col:end_col]) != 0), ]
  data2$tag <- 1:nrow(data2)
  sets <- list()
  intersections <- list()
  box_plot_data <- data.frame()
  for(i in 1:nrow(data1)){
   sets[[i]] <- colnames(data1)[which(data1[i, 1:length(names)] == 0)]
  }
  for(i in 1:length(sets)){
     intersections[[i]] <- data2[(rowSums(data2[ ,start_col:end_col]) == (length(names) - length(as.character(sets[[i]])))), ]
     intersections[[i]] <- Wanted(intersections[[i]], as.character(sets[[i]]))
     end <- ((start_col + (length(names) - length(as.character(sets[[i]]))))-1)
     if(start_col == end){
       intersections[[i]] <- intersections[[i]][(intersections[[i]][ ,start_col]) == 1, ]
       intersections[[i]] <- intersections[[i]]$tag
     }
     else{
       num <- length(names) - length(as.character(sets[[i]]))
       intersections[[i]] <- intersections[[i]][(rowSums(intersections[[i]][ ,start_col:end]) == num), ]
       intersections[[i]] <- intersections[[i]]$tag
     }
     intersections[[i]] <- data2[data2$tag %in% as.numeric(intersections[[i]]), ]
     intersections[[i]]$x <- i
   }
  for(i in 1:length(intersections)){
   box_plot_data <- rbind(box_plot_data, intersections[[i]])
  }
  return(box_plot_data)
}

BoxPlotsPlot <- function(bdat, att, att_color){
  yaxis <- as.character(att)
  col <- match(att, colnames(bdat))
  colnames(bdat)[col] <- "attribute"
  upper_xlim <- as.numeric((max(bdat$x) + 1))
  plot_lims <- as.numeric(0:upper_xlim)
boxplots <- ggplotGrob(ggplot() 
                       + theme_bw() +ylab(yaxis)
                       + scale_x_discrete(limits = plot_lims, expand = c(0,0))
                       + theme(plot.margin = unit(c(-0.7,0,0,0), "cm"), 
                               axis.title.y = element_text(vjust = -0.8),
                               axis.ticks.x = element_blank(),
                               axis.text.x = element_blank(),
                               panel.border = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.grid.major = element_blank(),
                               axis.title.x = element_blank())
                       + geom_boxplot(data = bdat, aes(x=factor(x), y=attribute),
                                      fill = att_color, colour = "gray80"))
return(boxplots)
}