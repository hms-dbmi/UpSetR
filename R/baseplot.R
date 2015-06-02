#' Base plot for UpSetR
#' 
#' Takes intersections from most abundant sets in a data set and produces a matrix
#' layout of the intersections, along with plotting their corresponding sizes and 
#' the size of each set used.
#' @param data Data set
#' @param first.col First column in data set that represents a set
#' @param last.col Last column in data set that represents a set
#' @param nsets Number of sets to look at
#' @param nintersects Number of intersections to plot
#' @param sets Specific sets to look at (Include as combinations. Ex: c("Name1", "Name2"))
#' @param matrix.color Color of the intersection points
#' @param main.bar.color Color of the main bar plot
#' @param sets.bar.color Color of set size bar plot
#' @param point.size Size of points in matrix plot
#' @param line.size Width of lines in matrix plot
#' @param name.size Size of set names in matrix plot
#' @param mb.ratio Ratio between matrix plot and main bar plot (Keep in terms of hundreths)
#' @param att.x Attribute entered as a string. If att.y is NULL a histogram will be produced. Best if data attached
#' @param att.y Attribute entered as a string. Produces a scatter plot vs. att.x. Best if data attached
#' @param expression Expression to subset attribute data entered as string (Ex: "ColName > 3"). Best if data attached
#' @param att.pos Position of attribute plot. If NULL plot will be at bottom. If "top" it will be above other plots
#' @param att.color Color of attribute plot bars or points
#' @export
upset_base <- function(data, first.col, last.col, nsets = 5, nintersects = 40, sets = NULL,
                       matrix.color = "gray23",main.bar.color = "gray23", sets.bar.color = "dodgerblue",
                       point.size = 4, line.size = 1, name.size = 10, mb.ratio = c(0.70,0.30), att.x = NULL, 
                       att.y = NULL, expression = NULL, att.pos = NULL, att.color = "dodgerblue"){
  Set_names <- sets
  if(is.null(Set_names) == T){
    Set_names <- FindMostFreq(data, first.col, last.col, nsets)
  }
  Sets_to_remove <- Remove(data, first.col, last.col, Set_names)
  New_data <- Wanted(data, Sets_to_remove)
  Num_of_set <- Number_of_sets(Set_names)
  All_Freqs <- Counter(New_data, Num_of_set, first.col, Set_names, nintersects)
  Matrix_setup <- Create_matrix(All_Freqs)
  labels <- Make_labels(Matrix_setup)
  Matrix_layout <- Create_layout(Matrix_setup, matrix.color)
  Set_sizes <- FindSetFreqs(New_data, first.col, Num_of_set)
  Main_bar <- Make_main_bar(All_Freqs, main.bar.color)
  Matrix <- Make_matrix_plot(Matrix_layout, Set_sizes, All_Freqs, point.size, line.size,
                             name.size, labels)
  Sizes <- Make_size_plot(Set_sizes, sets.bar.color)
  Make_base_plot(Main_bar, Matrix, Sizes, labels, mb.ratio, att.x, att.y, New_data,
                 expression, att.pos, first.col, att.color)
}

FindMostFreq <- function(data, start_col, end_col, n_sets){  
  temp_data <- data[ ,start_col:end_col]
  temp_data <- colSums(temp_data)
  temp_data <- as.data.frame(temp_data)
  temp_data <- tail(temp_data[order(temp_data[ ,"temp_data"]), , drop = F], as.integer(n_sets))
  temp_data <- rev(row.names(temp_data))
  return(temp_data)
}

Remove <- function(data, start_col, end_col, sets){
  temp_data <- as.data.frame(data[ , start_col:end_col])
  Unwanted_sets <- colnames(temp_data[ ,!(colnames(temp_data) %in% sets), drop = F])
}

Wanted <- function(data, unwanted_sets){
  temp_data <- (data[ ,!(colnames(data) %in% unwanted_sets), drop = F])
}

Subset_att <- function(data, exp){
  subset_data <- data[which(eval(parse(text = exp))), ]
  return(subset_data)
}

Number_of_sets <- function(sets){
  temp <- length(sets)
  return(temp)
}

Counter <- function(data, num_sets, start_col, name_of_sets, nintersections){
  temp_data <- list()
  Freqs <- data.frame()
  end_col <- as.numeric(((start_col + num_sets) -1))
  for( i in 1:num_sets){
    temp_data[i] <- match(name_of_sets[i], colnames(data))
  }
  Freqs <- count(data[ ,as.integer(temp_data)])
  Freqs <- Freqs[!(rowSums(Freqs[ ,1:num_sets]) == 0), ]
  Freqs <- Freqs[order(Freqs$freq, decreasing = T), ]
  for( i in 1:nrow(Freqs)){
    Freqs$x[i] <- i
  }
  Freqs <- Freqs[1:nintersections, ]
  Freqs <- na.omit(Freqs)
  return(Freqs)
}

Create_matrix <- function(data){
  Matrix_setup <- as.matrix(t(data[ , 1:(length(data) -2)]))
  names <- rownames(Matrix_setup)
  max <- max(nchar(names))
  if( max < 7)
  {
    Spaces <- list()
    for(i in 1:nrow(Matrix_setup)){
      Name_length <- nchar(names[i])
      Spaces_needed <- (6 - (Name_length))
      Spaces[i] <- paste(replicate(Spaces_needed, " "), collapse = "")
      rownames(Matrix_setup)[i] <- paste(as.character(Spaces[i]), names[i], collapse = "")
    }
    rownames(Matrix_setup) <- gsub(x = rownames(Matrix_setup), pattern = "\\.", replacement = " ")
  }
  return(Matrix_setup)
}

Make_labels <- function(setup){
  names <- rownames(setup)
  return(names)
}

Create_layout <- function(setup, mat_color){
  Matrix_layout <- expand.grid(y=seq(nrow(setup)), x=seq(ncol(setup)))
  Matrix_layout <- data.frame(Matrix_layout, value = as.vector(setup))
  for(i in 1:nrow(Matrix_layout)){
    if(Matrix_layout$value[i] == as.integer(1)){
      Matrix_layout$color[i] <- mat_color
      Matrix_layout$Intersection[i] <- paste(Matrix_layout$x[i], "yes", sep ="")
    }
    else{
      Matrix_layout$color[i] <- "gray92"
      Matrix_layout$Intersection[i] <- paste(i, "No", sep = "")
    } 
  }
  return(Matrix_layout)
}

FindSetFreqs <- function(data, start_col, num_sets){
  end_col <- as.numeric(((start_col + num_sets) -1))
  temp_data <- data[ ,start_col:end_col]
  temp_data <- as.data.frame(colSums(temp_data))
  temp_data <- temp_data[order(temp_data, decreasing = T), ]
  x <- seq(1:num_sets)
  temp_data <- cbind(temp_data, x)
  colnames(temp_data) <- c("y", "x")
  return(as.data.frame(temp_data))
}

Make_main_bar <- function(Main_bar_data, mbar_color){
  Main_bar_plot <- (ggplot(data = Main_bar_data, aes(x = x, y = freq)) 
                    + geom_bar(stat = "identity", colour = mbar_color, fill = mbar_color,
                               width = 0.4)
                    + scale_x_continuous(limits = c(0,(nrow(Main_bar_data)+1 )), expand = c(0,0),
                                         breaks = NULL)
                    + xlab(NULL) + ylab("Intersection Size")
                    + theme(panel.background = element_rect(fill = "white"),  
                            plot.margin=unit(c(0.35,0.2,0.2,0.2), "cm"),
                            panel.border = element_blank(),
                            axis.title.y = element_text(vjust = 0.5))
                    + geom_vline(xintercept = 0, size = 1, colour = "gray0")
                    + geom_hline( yintercept = 0, colour = "gray0")
                    + geom_text(aes(label = freq), size = 2.9, vjust = -0.4, colour = mbar_color))
  Main_bar_plot <- ggplotGrob(Main_bar_plot)
  return(Main_bar_plot)
}

Make_matrix_plot <- function(Mat_data,Set_size_data, Main_bar_data, point_size, line_size, name_size, labels){
  Matrix_plot <- (ggplot(data=Mat_data, aes(x= x, y= y)) 
                  + geom_point(colour = Mat_data$color, size= point_size) 
                  + geom_line(aes(group = Intersection), size = line_size, colour = Mat_data$color)
                  + theme(panel.background = element_rect(fill = "white"),
                          plot.margin=unit(c(-0.1,0.2,0.1,0.2), "cm"),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank(),
                          axis.text.y = element_text(colour = "gray0", size = name_size),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank())
                  + xlab(NULL) + ylab("   ")
                  + scale_y_continuous(breaks = c(1:nrow(Set_size_data)),
                                       limits = c(0.5,(nrow(Set_size_data) +0.5)),
                                       labels = labels)
                  + scale_x_continuous(limits = c(0,(nrow(Main_bar_data)+1 )), expand = c(0,0)))
  
  Matrix_plot <- ggplotGrob(Matrix_plot)
  return(Matrix_plot)
}

Make_size_plot <- function(Set_size_data, sbar_color){
  Size_plot <- (ggplot(data = Set_size_data, aes(x =x, y = y))
                + geom_bar(stat = "identity",colour = sbar_color, width = 0.4,
                           fill = sbar_color, position = "identity")
                + scale_x_continuous(limits = c(0.5, (nrow(Set_size_data)+0.5)),
                                     breaks = c(0, max(Set_size_data)))
                + theme(panel.background = element_rect(fill = "white"),
                        plot.margin=unit(c(0,0.2,0.1,0.2), "cm"),
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
                           Set_data, exp, position, start_col, att_color){
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
    values <- eval(parse(text = att_x))
    Set_data <- cbind(Set_data, values) 
    end_col <- ((start_col + as.integer(length(labels))) - 1)
    Set_data <- Set_data[which(rowSums(Set_data[ ,start_col:end_col]) != 0), ]
    if(is.null(exp) == F){
      attach(Set_data)
      Set_data <- Subset_att(Set_data, exp)
      detach(Set_data)
    }
    att_plot <- (ggplot(data = Set_data, aes(x = values)) 
                 + geom_histogram(binwidth = 1, colour = "black", fill = att_color)
                 + xlab(att_x) + ylab("Frequency")
                 + theme(panel.background = element_rect(fill = "white"),
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         plot.margin=unit(c(-0.1,0.2,0.1,0.2), "cm")))
    
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
    values1 <- eval(parse(text = att_x))
    values2 <- eval(parse(text = att_y))
    Set_data <- cbind(Set_data, values1, values2) 
    end_col <- ((start_col + as.integer(length(labels))) - 1)
    Set_data <- Set_data[which(rowSums(Set_data[ ,start_col:end_col]) != 0), ]
    if(is.null(exp) == F){
      attach(Set_data)
      Set_data <- Subset_att(Set_data, exp)
      detach(Set_data)
    }
    att_plot <- (ggplot(data = Set_data, aes(x = values1, y = values2)) 
                 + geom_point(colour = att_color)
                 + xlab(att_x) + ylab(att_y)
                 + theme(panel.background = element_rect(fill = "white"),
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         plot.margin=unit(c(0.2,0.2,0.1,0.2), "cm")))
    
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