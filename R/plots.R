Make_main_bar <- function(Main_bar_data, Q, show_num, ratios){
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
                            axis.title.y = element_text(vjust = 0.5)))
  if((show_num == "yes") || (show_num == "Yes")){
    Main_bar_plot <- (Main_bar_plot + geom_text(aes(label = freq), size = 3.0, vjust = -0.4, colour = Main_bar_data$color))
  }
  if(is.null(inter_data) == F){
    Main_bar_plot <- Main_bar_plot + geom_bar(data = inter_data,
                                              aes(x=x, y = freq), colour = inter_data$color, fill = inter_data$color,
                                              stat = "identity", width = 0.4)
    if((show_num == "yes") || (show_num == "Yes")){
      Main_bar_plot <- (Main_bar_plot + geom_text(data = inter_data, aes(label = freq), size = 3.0, 
                                                  vjust = -0.4, colour = inter_data$color))
    }
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
  data$min <- 0.5
  data$max <- (max(Mat_data$x) + 0.5)
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
                          axis.text.y = element_text(colour = "gray0", size = name_size),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank())
                  + xlab(NULL) + ylab("   ")
                  + scale_y_continuous(breaks = c(1:nrow(Set_size_data)),
                                       limits = c(0.5,(nrow(Set_size_data) +0.5)),
                                       labels = labels)
                  + scale_x_continuous(limits = c(0,(nrow(Main_bar_data)+1 )), expand = c(0,0))
                  + geom_rect(data = shading_data, aes(xmin = min, xmax = max, ymin = y_min, ymax = y_max ),
                              fill = shade_color, alpha = shade_alpha)
                  + geom_point(data=Mat_data, aes(x= x, y= y), colour = Mat_data$color, size= point_size)
                  + geom_line(data = Mat_data, aes(group = Intersection, x=x, y=y), 
                              size = line_size, colour = Mat_data$color))
  Matrix_plot <- ggplotGrob(Matrix_plot)
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
                                     breaks = c(0, max(Set_size_data)))
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

IntersectionBoxPlot <- function(data1, data2){
  View(data)
  View(data2)
}


Make_base_plot <- function(Main_bar_plot, Matrix_plot, Size_plot, labels, hratios, att_x, att_y,
                           Set_data, exp, position, start_col, att_color, elems_att, q_att, q,
                           Q_Title){
  
  Main_bar_plot$widths <- Matrix_plot$widths
  Matrix_plot$heights <- Size_plot$heights 
  
  size_plot_height <- (((hratios[1])+0.01)*100) 
  if((hratios[1] > 0.7 || hratios[1] < 0.3) || 
       (hratios[2] > 0.7 || hratios[2] < 0.3)) warning("Plot might be out of range if ratio > 0.7 or < 0.3")
  vplayout <- function(x,y){
    viewport(layout.pos.row = x, layout.pos.col = y)
  }
  if((is.null(att_x) == T) && (is.null(att_y) == F)){
    warning("Please place lone attribute in att.x")
    if(is.null(exp) == F) warning("No attribute selected to subset")
  }
  
  else if((is.null(att_x) == T) && (is.null(att_y) == T)){
    if(is.null(exp) == F) warning("No attribute selected to subset")
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(100,100)))
    print(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios), vp = vplayout(1:100, 21:100))
    print(arrangeGrob(Size_plot), vp = vplayout(size_plot_height:100, 1:20))
  }
  
  else if((is.null(att_x) == F) && (is.null(att_y) == T)){
    col_to_switch <- match(att_x, colnames(Set_data))
    end_col <- ((start_col + as.integer(length(labels))) - 1)
    Set_data <- Set_data[which(rowSums(Set_data[ ,start_col:end_col]) != 0), ]
    if(is.null(exp) == F){
      Set_data <- Subset_att(Set_data, exp)
    }
    colnames(Set_data)[col_to_switch] <- "values"
    if(is.null(q) == F){
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
    if(is.null(q) == F){
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
                         plot.margin=unit(c(-0.7,0.2,0.1,0.2), "cm")))
    if(is.null(elems) == F){
      for(i in 1:length(EColors)){
        Color <- EColors[i]
        elems_data <- elems[which(elems$color == Color), ]
        att_plot <- att_plot + geom_histogram(data = elems_data, aes(x = val1), 
                                              binwidth = 1, colour = "black", fill = Color)
      }
    }
    if(is.null(intersect) == F){
      for(i in 1:length(IColors)){
        Color <- IColors[i]
        intersect_data <- intersect[which(intersect$IColor == Color ), ]
        att_plot <- att_plot + geom_histogram(data = intersect_data, aes(x = v1), binwidth = 1,
                                              colour = "black", fill = Color)
      }
    }
    
    att_plot <- ggplot_gtable(ggplot_build(att_plot))
    att_plot$widths <-  Matrix_plot$widths
    if((hratios[1] < 0.4) || 
         (hratios[2] > 0.6)) warning("Plot might be out of range if mb.ratio[1] < 0.4 or mb.ratio[2] >  0.6")
    if(is.null(position) == T){
      size_plot_height <- (((hratios[1])+0.01)*100) 
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(130, 100)))
      print(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios), vp = vplayout(1:100, 21:100))
      print(arrangeGrob(Size_plot), vp = vplayout(size_plot_height:100, 1:20))
      print(arrangeGrob(att_plot), vp = vplayout(101:130, 21:100))
    }
    else{
      size_plot_height <- ((((hratios[1])+0.01)*100) + 30) 
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(130, 100)))
      print(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios), vp = vplayout(31:130, 21:100))
      print(arrangeGrob(Size_plot), vp = vplayout(size_plot_height:130, 1:20))
      print(arrangeGrob(att_plot), vp = vplayout(1:30, 21:100))
    }
  }
  else if((is.null(att_x) == F) && (is.null(att_y) == F)){
    col_switch1 <- match(att_x, colnames(Set_data))
    col_switch2 <- match(att_y, colnames(Set_data))
    end_col <- ((start_col + as.integer(length(labels))) - 1)
    Set_data <- Set_data[which(rowSums(Set_data[ ,start_col:end_col]) != 0), ]
    if(is.null(exp) == F){
      Set_data <- Subset_att(Set_data, exp)
    }
    colnames(Set_data)[col_switch1] <- "values1"
    colnames(Set_data)[col_switch2] <- "values2"
    if(is.null(q) == F){
      elems <- elems_att
      if(nrow(elems) == 0){
        elems <- NULL
      } 
    }
    else{
      elems <- NULL
    }
    if(is.null(q) == F){
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
                         plot.margin=unit(c(-0.7,0.2,0.1,0.2), "cm")))
    if(is.null(elems) == F){
      att_plot <- att_plot + geom_point(data = elems, aes(x = val1, y = val2), colour = elems$color)
    }
    if(is.null(intersect) == F){
      att_plot <- (att_plot + geom_point(data = intersect, aes(x = v1, y = v2), color = intersect$color))
    }
    
    att_plot <- ggplot_gtable(ggplot_build(att_plot))
    att_plot$widths <-  Matrix_plot$widths
    if((hratios[1] < 0.4) || 
         (hratios[2] > 0.6)) warning("Plot might be out of range if mb.ratio[1] < 0.4 or mb.ratio[2] >  0.6")
    if(is.null(position) == T){
      size_plot_height <- (((hratios[1])+0.01)*100) 
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(130, 100)))
      print(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios), vp = vplayout(1:100, 21:100))
      print(arrangeGrob(Size_plot), vp = vplayout(size_plot_height:100, 1:20))
      print(arrangeGrob(att_plot), vp = vplayout(101:130, 21:100))
    }
    else{
      size_plot_height <- ((((hratios[1])+0.01)*100) + 30) 
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(130, 100)))
      print(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios), vp = vplayout(31:130, 21:100))
      print(arrangeGrob(Size_plot), vp = vplayout(size_plot_height:130, 1:20))
      print(arrangeGrob(att_plot), vp = vplayout(1:30, 21:100))
    }
  }
}